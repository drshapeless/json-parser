// This json parser does not support json with comment and trailing comma.

Json_Type :: enum u8 {
    NULL;
    BOOLEAN;
    NUMBER;
    STRING;
    ARRAY;
    OBJECT;
}

Json_Any :: struct {
    type: Json_Type;
    union {
        boolean: bool;
        number: float64;
        str: string;
        array: [..]Json_Any;
        object: Json_Object;
    }
}

Key_Value :: struct {
    key: string;
    value: Json_Any;
}

Json_Object :: [..]Key_Value;

Json_Result :: enum {
    OK;
    EXPECT_OBJECT_OR_ARRAY;
    EXPECT_KEY;
    EXPECT_COLON;
    EXPECT_VALUE;
    EXPECT_COMMA;
    ERROR_PARSING_NUMBER;
    ERROR_MISMATCH_BRACE;
    ERROR_MISMATCH_SQUARE_BRACKET;
    ERROR_NO_MATCHING_MEMBER;
    ERROR_NOT_INTEGER;
    ERROR_NOT_FLOAT;
    ERROR_NOT_BOOLEAN;
    ERROR_NOT_STRING;
    ERROR_NOT_STRUCT;
    ERROR_NOT_ARRAY;
    ERROR_NOT_NUMBER;
    ERROR_DATATYPE_NOT_SUPPORTED;
    ERROR_MISMATCH_FIXED_ARRAY_SIZE;
    ERROR_NO_MATCHING_STRUCT;
    ERROR_ARRAY_OF_ARRAY_NOT_SUPPORTED;
}

parse_json :: (s: string) -> Json_Any, Json_Result, error_position:int {
    tokens := tokenize(s);

    root: Json_Any;

    // In the last version, I was worry about the value pointing to a
    // dynamic array would be altered if the array expand. However,
    // the nature of json parsing is not going to expand a parent
    // container, the worry was redundant.
    stack: [..]*Json_Any;
    defer array_free(stack);

    array_add(*stack, *root);

    current_key_value: Key_Value;

    state: Parse_State;

    for tokens {
        if stack.count == 0 {
            break;
        }

        if #complete state == {
        case .UNINIT;
            if it.type == .OPEN_BRACE {
                root.type = .OBJECT;
                state = .KEY;
            } else if it.type == .OPEN_SQUARE_BRACKET {
                root.type = .ARRAY;
                state = .VALUE;
            } else {
                return .{}, .EXPECT_OBJECT_OR_ARRAY, it.raw_string.data - s.data;
            }
        case .KEY;
            if it.type == {
            case .QUOTED_STRING;
                current_key_value.key = unescape(unquote(it.raw_string));
                state = .COLON;
            case .CLOSE_BRACE;
                // Support for empty struct, e.g. {}
                parent := peek(stack);
                if parent.type == .OBJECT {
                    pop(*stack);
                    state = .COMMA_OR_END;
                } else {
                    return .{}, .ERROR_MISMATCH_BRACE, it.raw_string.data - s.data;
                }
            case;
                return .{}, .EXPECT_KEY, it.raw_string.data - s.data;
            }
        case .COLON;
            if it.type == .COLON {
                state = .VALUE;
            } else {
                return .{}, .EXPECT_COLON, it.raw_string.data - s.data;
            }
        case .COMMA_OR_END;
            if it.type == {
            case .COMMA;
                // Something to notice is that, by supporting empty
                // array and emtpy struct, in the key session, we now
                // in theory support trailing comma.
                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    state = .KEY;
                case .ARRAY;
                    state = .VALUE;
                }
            case .CLOSE_BRACE;
                parent := peek(stack);
                if parent.type == .OBJECT {
                    pop(*stack);

                    // Just poping the stack is enough, because the
                    // state is still .COMMA_OR_END when we finish a
                    // value.
                } else {
                    return .{}, .ERROR_MISMATCH_BRACE, it.raw_string.data - s.data;
                }
            case .CLOSE_SQUARE_BRACKET;
                parent := peek(stack);
                if parent.type == .ARRAY {
                    pop(*stack);
                } else {
                    return .{}, .ERROR_MISMATCH_SQUARE_BRACKET, it.raw_string.data - s.data;
                }
            case;
                log("% % % %", it, it_index, it.raw_string.data, s.data);
                return .{}, .EXPECT_COMMA, it.raw_string.data - s.data;
            }
        case .VALUE;
            if it.type == {
            case .QUOTED_STRING;
                v: Json_Any;
                v.type = .STRING;
                v.str = unescape(unquote(it.raw_string));
                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    current_key_value.value = v;
                    array_add(*parent.object, current_key_value);
                    current_key_value = .{};
                case .ARRAY;
                    array_add(*parent.array, v);
                case;
                    assert(false);
                }
                state = .COMMA_OR_END;

            case .UNQUOTED_STRING;
                v, ok := parse_non_string(it.raw_string);
                if !ok {
                    return .{}, .ERROR_PARSING_NUMBER, it.raw_string.data - s.data;
                }

                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    current_key_value.value = v;
                    array_add(*parent.object, current_key_value);
                    current_key_value = .{};
                case .ARRAY;
                    array_add(*parent.array, v);
                case;
                    assert(false);
                }
                state = .COMMA_OR_END;

            case .OPEN_BRACE;
                v: Json_Any;
                v.type = .OBJECT;

                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    current_key_value.value = v;
                    array_add(*parent.object, current_key_value);
                    current_key_value = .{};

                    pv := peek_pointer(parent.object);
                    array_add(*stack, *pv.value);
                case .ARRAY;
                    array_add(*parent.array, v);
                    pv := peek_pointer(parent.array);
                    array_add(*stack, pv);
                case;
                    assert(false);
                }

                state = .KEY;
            case .OPEN_SQUARE_BRACKET;
                v: Json_Any;
                v.type = .ARRAY;

                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    current_key_value.value = v;
                    array_add(*parent.object, current_key_value);
                    current_key_value = .{};

                    pv := peek_pointer(parent.object);
                    array_add(*stack, *pv.value);
                case .ARRAY;
                    array_add(*parent.array, v);
                    pv := peek_pointer(parent.array);
                    array_add(*stack, pv);
                case;
                    assert(false);
                }

                state = .VALUE;

            case .CLOSE_SQUARE_BRACKET;
                // In some rare case, the value is an empty array, e.g. []
                // Just pop the current array from the stack
                parent := peek(stack);
                if parent.type == .ARRAY {
                    pop(*stack);
                    state = .COMMA_OR_END;
                } else {
                    return .{}, .ERROR_MISMATCH_SQUARE_BRACKET, it.raw_string.data - s.data;
                }
            case;
                return .{}, .EXPECT_VALUE, it.raw_string.data - s.data;
            }
        }
    }

    return root, .OK, 0;
}

json_free :: (json: Json_Any) {
    if json.type == {
    case .ARRAY;
        for json.array {
            json_free(it);
        }
        array_free(json.array);
    case .OBJECT;
        for json.object {
            free(it.key);
            json_free(it.value);
        }
        array_free(json.object);
    case .STRING;
        free(json.str);
    }
}

json_print :: (sb: *String_Builder, value: Json_Any, indent: bool = false, indent_level: int = 0, indent_space: int = 4) {
    if #complete value.type == {
    case .NULL;
        append(sb, "null");
    case .BOOLEAN;
        if value.boolean {
            append(sb, "true");
        } else {
            append(sb, "false");
        }
    case .NUMBER;
        print_float(sb, formatFloat(value.number));
    case .STRING;
        append(sb, "\"");
        append(sb, value.str);
        append(sb, "\"");
    case .ARRAY;
        append(sb, "[");
        if indent append(sb, "\n");
        for value.array {
            if indent {
                repeat(sb, " ", (indent_level + 1) * indent_space);
            }
            json_print(sb, it, indent, indent_level + 1, indent_space);
            if it_index != value.array.count - 1 {
                append(sb, ", ");
            }
            if indent {
                append(sb, "\n");
            }
        }

        if indent {
            repeat(sb, " ", indent_level * indent_space);
        }
        append(sb, "]");
    case .OBJECT;
        append(sb, "{");
        if indent append(sb, "\n");
        for value.object {
            if indent {
                repeat(sb, " ", (indent_level + 1) * indent_space);
            }
            append(sb, "\"");
            append(sb, it.key);
            append(sb, "\"");
            append(sb, ": ");
            json_print(sb, it.value, indent, indent_level + 1, indent_space);
            if it_index != value.object.count - 1 {
                append(sb, ", ");
            }
            if indent {
                append(sb, "\n");
            }
        }

        if indent {
            repeat(sb, " ", indent_level * indent_space);
        }
        append(sb, "}");
    }
}

json_print :: (value: Json_Any, indent: bool = false, indent_level: int = 0, indent_space: int = 4) -> string {
    sb: String_Builder;
    json_print(*sb, value, indent, indent_level, indent_space);
    return builder_to_string(*sb);
}

// Return Json_Any object with type NULL if nothing is matched.
json_get :: (value: Json_Any, key: string) -> Json_Any, ok:bool {
    if value.type != .OBJECT return .{}, false;

    for value.object {
        if it.key == key {
            return it.value, true;
        }
    }

    return .{}, false;
}

// From Jai data type to json, only bool, int, float, string, struct
// and array are supported. Additionally, supports Anyx type for
// better binding generation.
to_json :: (data: *void, ti: *Type_Info) -> Json_Any, ok:bool {
    root: Json_Any;

    if ti.type == {
    case .INTEGER;
        root.type = .NUMBER;
        t := cast(*Type_Info_Integer)ti;
        if t.signed {
            if ti.runtime_size == {
            case 1;
                root.number = xx (cast(*s8)data).*;
            case 2;
                root.number = xx (cast(*s16)data).*;
            case 4;
                root.number = xx (cast(*s32)data).*;
            case 8;
                root.number = xx (cast(*s64)data).*;
            }
        } else {
            if ti.runtime_size == {
            case 1;
                root.number = xx (cast(*u8)data).*;
            case 2;
                root.number = xx (cast(*u16)data).*;
            case 4;
                root.number = xx (cast(*u32)data).*;
            case 8;
                root.number = xx (cast(*u64)data).*;
            }
        }
    case .FLOAT;
        root.type = .NUMBER;
        t := cast(*Type_Info_Float)ti;
        if t.runtime_size == {
        case 4;
            root.number = xx (cast(*float32)data).*;
        case 8;
            root.number = (cast(*float64)data).*;
        }
    case .BOOL;
        root.type = .BOOLEAN;
        root.boolean = (cast(*bool)data).*;
    case .STRING;
        root.type = .STRING;
        root.str = escape((cast(*string)data).*);
    case .STRUCT;
        if ti == type_info(Anyx) {
            a := cast(*Anyx)data;
            if a.optional && !a.has_value {
                root.type = .NULL;
            } else {
                v, ok := to_json(a.value.value_pointer, a.value.type);
                return v, ok;
            }
        } else {
            t := cast(*Type_Info_Struct)ti;
            root.type = .OBJECT;
            for t.members {
                kv: Key_Value;
                // Assume the member name is an ascii string.
                kv.key = copy_string(it.name);

                v, ok := to_json(cast(*u8)data + it.offset_in_bytes, it.type);
                if !ok {
                    return .{}, false;
                }
                kv.value = v;

                array_add(*root.object, kv);
            }
        }

    case .ARRAY;
        t := cast(*Type_Info_Array)ti;
        root.type = .ARRAY;

        arr_view := cast(*Array_View_64)data;
        for 0..arr_view.count - 1 {
            v, ok := to_json(cast(*u8)arr_view.data + t.element_type.runtime_size * it, t.element_type);
            if !ok {
                return .{}, false;
            }

            array_add(*root.array, v);
        }
    case;
        return .{}, false;
    }

    return root, true;
}

to_json :: (obj: *$T) -> Json_Any, ok:bool {
    ti := type_info(T);

    j, success := to_json(obj, ti);
    return j, success;
}

from_json :: (value: Json_Any, data: *void, ti: *Type_Info) -> Json_Result {
    if ti.type == {
    case .INTEGER;
        if value.type == .NUMBER {
            success := from_json_integer(value, data, ti);
            if success return .OK;
        }
        return .ERROR_NOT_INTEGER;
    case .FLOAT;
        if value.type == .NUMBER {
            success := from_json_float(value, data, ti);
            if success return .OK;
        }
        return .ERROR_NOT_FLOAT;
    case .BOOL;
        if value.type == .BOOLEAN {
            (cast(*bool)data).* = value.boolean;
            return .OK;
        } else {
            return .ERROR_NOT_BOOLEAN;
        }
    case .STRING;
        if value.type == .STRING {
            (cast(*string)data).* = copy_string(value.str);
            return .OK;
        } else {
            return .ERROR_NOT_STRING;
        }
    case .STRUCT;
        if ti == type_info(Anyx) {
            a := cast(*Anyx)data;
            if a.optional {
                if value.type == .NULL {
                    a.has_value = false;
                    return .OK;
                } else {
                    a.has_value = true;
                }
            }

            if a.accepted_types.count == 1 {
                ti2 := a.accepted_types[0];
                a.value.value_pointer = alloc(ti2.runtime_size);
                return from_json(value, a.value.value_pointer, ti2);
            } else {
                // case .NULL would never happen
                if value.type == {
                case .BOOLEAN;
                    // Check if there is a bool in the accepted types
                    found := false;
                    found_type_info: *Type_Info;
                    for a.accepted_types {
                        if it.type == .BOOL {
                            found = true;
                            found_type_info = it;
                        }
                    }
                    if !found {
                        return .ERROR_NOT_BOOLEAN;
                    }
                    a.value.value_pointer = alloc(found_type_info.runtime_size);
                    a.value.type = found_type_info;
                    memcpy(a.value.value_pointer, *value.boolean, found_type_info.runtime_size);
                    return .OK;
                case .NUMBER;
                    // Check if there is a number type
                    found := false;
                    found_type_info: *Type_Info;
                    for a.accepted_types {
                        if it.type == .INTEGER || it.type == .FLOAT {
                            found = true;
                            found_type_info = it;
                        }
                    }
                    if !found {
                        return .ERROR_NOT_NUMBER;
                    }
                    a.value.value_pointer = alloc(found_type_info.runtime_size);
                    a.value.type = found_type_info;
                    if found_type_info.type == .INTEGER {
                        success := from_json_integer(value, a.value.value_pointer, a.value.type);
                        if !success return .ERROR_NOT_INTEGER;
                    } else if found_type_info.type == .FLOAT {
                        success := from_json_float(value, a.value.value_pointer, a.value.type);
                        if !success return .ERROR_NOT_FLOAT;
                    }
                    return .OK;
                case .STRING;
                    // Check if there is a string type
                    found := false;
                    found_type_info: *Type_Info;
                    for a.accepted_types {
                        if it.type == .STRING {
                            found = true;
                            found_type_info = it;
                        }
                    }
                    if !found {
                        return .ERROR_NOT_STRING;
                    }
                    a.value.value_pointer = alloc(size_of(string));
                    a.value.type = found_type_info;
                    (cast(*string)a.value.value_pointer).* = copy_string(value.str);
                    return .OK;
                case .ARRAY;
                    // Dealing with array is tricky, because we have
                    // to also consider the type of the array element.
                    number_of_accepted_array := 0;
                    selected_array_type_infos: [..]*Type_Info;
                    defer array_free(selected_array_type_infos);
                    for a.accepted_types {
                        if it.type == .ARRAY {
                            number_of_accepted_array += 1;
                            array_add(*selected_array_type_infos, it);
                        }
                    }

                    if number_of_accepted_array == 1 {
                        // This is the best case
                        a.value.value_pointer = alloc(selected_array_type_infos[0].runtime_size);
                        a.value.type = selected_array_type_infos[0];
                        return from_json_array(value, a.value.value_pointer, selected_array_type_infos[0]);
                    } else if number_of_accepted_array > 1 {
                        if value.array.count == 0 {
                            // Since the array count is zero, we can't
                            // find out what type it is.
                            a.value.value_pointer = alloc(size_of(Array_View_64));
                            a.value.type = type_info(Array_View_64);
                        } else {
                            // Investigate the first element of the
                            // json array
                            selected_info: *Type_Info;
                            first_element := value.array[0];
                            if #complete first_element.type == {
                            case .NULL;
                                // What the fuck? Just consider it as a *void.
                                selected_info = type_info(*void);
                            case .BOOLEAN;
                                for selected_array_type_infos {
                                    t := cast(*Type_Info_Array)ti;
                                    if t.element_type.type == .BOOL {
                                        selected_info = it;
                                        break;
                                    }
                                }
                            case .NUMBER;
                                for selected_array_type_infos {
                                    t := cast(*Type_Info_Array)ti;
                                    if t.element_type.type == .FLOAT || t.element_type.type == .INTEGER {
                                        selected_info = it;
                                        break;
                                    }
                                }
                            case .STRING;
                                for selected_array_type_infos {
                                    t := cast(*Type_Info_Array)ti;
                                    if t.element_type.type == .STRING {
                                        selected_info = it;
                                        break;
                                    }
                                }
                            case .ARRAY;
                                return .ERROR_ARRAY_OF_ARRAY_NOT_SUPPORTED;
                            case .OBJECT;
                                obj_hash := hash_json_object(first_element);
                                for selected_array_type_infos {
                                    t := cast(*Type_Info_Array)ti;
                                    struct_hash := hash_struct_info(t);
                                    if obj_hash == struct_hash {
                                        selected_info = it;
                                        break;
                                    }
                                }
                            }

                            if selected_info == null {
                                return .ERROR_NO_MATCHING_STRUCT;
                            }

                            a.value.value_pointer = alloc(selected_info.runtime_size);
                            a.value.type = selected_info;

                            return from_json_array(value, a.value.value_pointer, a.value.type);
                        }
                    } else {
                        return .ERROR_NOT_ARRAY;
                    }
                case .OBJECT;
                    number_of_accepted_struct := 0;
                    struct_infos: [..]*Type_Info;
                    defer array_free(struct_infos);
                    for a.accepted_types {
                        if it.type == .STRUCT {
                            number_of_accepted_struct += 1;
                            array_add(*struct_infos, it);
                        }
                    }

                    if number_of_accepted_struct == 1 {
                        a.value.value_pointer = alloc(struct_infos[0].runtime_size);
                        a.value.type = struct_infos[0];
                        return from_json(value, a.value.value_pointer, a.value.type);
                    } else if number_of_accepted_struct > 1 {
                        json_obj_hash := hash_json_object(value);
                        selected_info: *Type_Info;
                        for struct_infos {
                            if json_obj_hash == hash_struct_info(it) {
                                selected_info = it;
                            }
                        }

                        if selected_info == null {
                            return .ERROR_NO_MATCHING_STRUCT;
                        }

                        a.value.value_pointer = alloc(selected_info.runtime_size);
                        a.value.type = selected_info;
                        return from_json(value, a.value.value_pointer, a.value.type);
                    } else {
                        return .ERROR_NOT_STRUCT;
                    }
                }
            }
        } else if value.type == .OBJECT {
            t := cast(*Type_Info_Struct)ti;
            for t.members {
                v, ok := json_get(value, it.name);
                // If the current member is not found, forget about it.
                if !ok continue;

                result := from_json(v, cast(*u8)data + it.offset_in_bytes, it.type);
                if result != .OK return result;
            }

            return .OK;
        } else {
            return .ERROR_NOT_STRUCT;
        }
    case .ARRAY;
        if value.type == .ARRAY {
            return from_json_array(value, data, ti);
        } else {
            return .ERROR_NOT_ARRAY;
        }
    }

    return .ERROR_DATATYPE_NOT_SUPPORTED;
}

from_json :: (value: Json_Any, obj: *$T) -> Json_Result {
    return from_json(value, obj, type_info(T));
}


#scope_file
Token_Type :: enum u8 {
    OPEN_BRACE;
    CLOSE_BRACE;
    COLON;
    COMMA;
    QUOTED_STRING;
    UNQUOTED_STRING;
    OPEN_SQUARE_BRACKET;
    CLOSE_SQUARE_BRACKET;
}

Token :: struct {
    type: Token_Type;
    raw_string: string;
}

tokenize :: (s: string) -> []Token {
    tokens: [..]Token;

    i := 0;
    while i < s.count {
        c := s[i];
        if is_space(c) {
            i += 1;
            continue;
        }

        t: Token;
        t.raw_string = .{
            data = s.data + i,
            count = 1,
        };

        if c == {
        case #char "{";
            t.type = .OPEN_BRACE;
        case #char "}";
            t.type = .CLOSE_BRACE;
        case #char "[";
            t.type = .OPEN_SQUARE_BRACKET;
        case #char "]";
            t.type = .CLOSE_SQUARE_BRACKET;
        case #char ":";
            t.type = .COLON;
        case #char ",";
            t.type = .COMMA;
        case #char "\"";
            // Find next double quote
            t.type = .QUOTED_STRING;

            new_i := i + 1;
            found := false;
            while !found {
                if s[new_i] == #char "\"" && s[new_i - 1] != "\\" {
                    found = true;
                    t.raw_string.count = new_i - i + 1;
                }
                new_i += 1;
            }
            i = new_i - 1;
        case;
            // unquoted string is here
            t.type = .UNQUOTED_STRING;

            // Find the next space, comma, or close brace
            new_i := i + 1;
            found := false;
            while !found {
                cc := s[new_i];
                if is_space(cc) || cc == #char "," || cc == #char "}" || cc == #char "]" {
                    found = true;
                    t.raw_string.count = new_i - i;
                } else {
                    new_i += 1;
                }
            }
            i = new_i - 1;
        }

        array_add(*tokens, t);
        i += 1;
    }

    return tokens;
}

Parse_State :: enum {
    UNINIT;
    KEY;
    COLON;
    VALUE;
    COMMA_OR_END;
}

unescape :: (s: string) -> string, success:bool {
    sb: String_Builder;

    i := 0;
    while i < s.count {
        c := s[i];
        if c == #char "\"" {
            i += 1;
            if i < s.count {
                if s[i] == {
                case #char "\\";
                    append(*sb, #char "\\");
                case #char "\"";
                    append(*sb, #char "\"");
                case #char "/";
                    // This is an optional escape in json.
                    append(*sb, #char "/");
                // case #char "b";
                //     // Backspace, rarely use.
                //     append(*sb, #char "\b");
                // case #char "f";
                //     append(*sb, #char "\f");
                case #char "n";
                    append(*sb, #char "\n");
                case #char "r";
                    append(*sb, #char "\r");
                case #char "t";
                    append(*sb, #char "\t");
                case #char "u";
                    // Nightmare
                    // To parse this, we have to ensure the next 4 chars are hex.
                    if i + 4 >= s.count {
                        return "", false;
                    }
                    result, success := string_to_int(slice(s, i + 1, 4), 16, u32);
                    if !success {
                        return "", false;
                    }

                    stack_data: [4]u8;
                    stack_string := cast(string)stack_data;
                    character_utf32_to_utf8(result, *stack_string);
                    append(*sb, stack_string);
                }
            }
        } else {
            append(*sb, c);
        }

        i += 1;
    }

    str := builder_to_string(*sb);

    return str, true;
}

unquote :: (s: string) -> string {
    if s.count > 0 {
        if s[0] == #char "\"" && s[s.count - 1] == #char "\"" {
            return slice(s, 1, s.count - 2);
        }
    }

    return s;
}

parse_non_string :: (s: string) -> Json_Any, ok:bool {
    v: Json_Any;
    if s == {
    case "true";
        v.type = .BOOLEAN;
        v.boolean = true;
    case "false";
        v.type = .BOOLEAN;
        v.boolean = false;
    case "null";
        v.type = .NULL;
    case;
        // Should be number.
        num, ok := string_to_float64(s);
        if !ok {
            return .{}, false;
        }
        v.type = .NUMBER;
        v.number = num;
    }

    return v, true;
}

// We don't escape slash.
escape :: (s: string) -> string {
    sb: String_Builder;

    for c: s {
        if c == {
        case #char "\"";
            #through;
        case #char "\\";
            #through;
        // case #char "\b";
        //     #through;
        // case #char "\f";
        //     #through;
        case #char "\n";
            #through;
        case #char "\r";
            #through;
        case #char "\t";
            append(*sb, "\\");
        }
        append(*sb, c);
    }

    return builder_to_string(*sb);
}

repeat :: (sb: *String_Builder, s: string, count: int) {
    n := count;
    while n > 0 {
        append(sb, s);
        n -= 1;
    }
}

from_json_integer :: (value: Json_Any, data: *void, ti: *Type_Info) -> success:bool {
    t := cast(*Type_Info_Integer)ti;
    if t.signed {
        if t.runtime_size == {
        case 1;
            (cast(*s8)data).* = xx value.number;
            return true;
        case 2;
            (cast(*s16)data).* = xx value.number;
            return true;
        case 4;
            (cast(*s32)data).* = xx value.number;
            return true;
        case 8;
            (cast(*s64)data).* = xx value.number;
            return true;
        }
    } else {
        if t.runtime_size == {
        case 1;
            (cast(*u8)data).* = xx value.number;
            return true;
        case 2;
            (cast(*u16)data).* = xx value.number;
            return true;
        case 4;
            (cast(*u32)data).* = xx value.number;
            return true;
        case 8;
            (cast(*u64)data).* = xx value.number;
            return true;
        }
    }

    return false;
}

from_json_float :: (value: Json_Any, data: *void, ti: *Type_Info) -> success:bool {
    if ti.runtime_size == {
    case 4;
        (cast(*float32)data).* = xx value.number;
        return true;
    case 8;
        (cast(*float64)data).* = value.number;
        return true;
    }

    return false;
}

from_json_array :: (value: Json_Any, data: *void, ti: *Type_Info) -> Json_Result {
    t := cast(*Type_Info_Array)ti;
    if #complete t.array_type == {
    case .FIXED;
        if t.array_count != value.array.count return .ERROR_MISMATCH_FIXED_ARRAY_SIZE;

        arr := cast(*Array_View_64)data;

        for 0..value.array.count - 1 {
            result := from_json(value.array[it], cast(*u8)arr.data + t.element_type.runtime_size * it, t.element_type);
            if result != .OK return result;
        }

        return .OK;
    case .VIEW;
        arr := cast(*Array_View_64)data;

        arr.data = alloc(t.element_type.runtime_size * value.array.count);
        arr.count = value.array.count;
        for 0..value.array.count - 1 {
            result := from_json(value.array[it], cast(*u8)arr.data + t.element_type.runtime_size * it, t.element_type);
            if result != .OK return result;
        }

        return .OK;
    case .RESIZABLE;
        arr := cast(*Resizable_Array)data;

        array_reserve(arr, value.array.count, t.element_type.runtime_size);
        arr.count = value.array.count;
        for 0..value.array.count - 1 {
            result := from_json(value.array[it], cast(*u8)arr.data + t.element_type.runtime_size * it, t.element_type);
            if result != .OK return result;
        }
        return .OK;
    }
}

// Here is a hash function to check which struct is the matching one.

// It is done by multiplying the sum of ascii value of each character
// and the length of string.
hash_json_object :: (value: Json_Any) -> int {
    ret: int;
    for o: value.object {
        num: int;
        for c: o.key {
            num += c;
        }

        num *= o.key.count;
        ret += num;
    }

    return ret;
}

hash_struct_info :: (ti: *Type_Info) -> int {
    t := cast(*Type_Info_Struct)ti;
    ret: int;
    for m: t.members {
        num: int;
        for c: m.name {
            num += c;
        }

        num *= m.name.count;
        ret += num;
    }

    return ret;
}

#import "Basic";
#import "String";
#import "Unicode";
#import,dir "./Anyx";
