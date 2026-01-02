#module_parameters(DEBUG := false); // Enable DEBUG for logging error

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

Json_Result :: enum u32 {
    OK;
    LEXER_ERROR;
    FILE_NOT_FOUND;

    // parse error
    EXPECT_OBJECT_OR_ARRAY;
    EXPECT_KEY;
    EXPECT_COLON;
    EXPECT_COMMA;
    EXPECT_VALUE;
    MISMATCH_BRACE;
    MISMATCH_BRACKET;

    // serialize error
    EXPECT_NUMBER;
    EXPECT_BOOLEAN;
    EXPECT_STRING;
    EXPECT_ARRAY;
    EXPECT_OBJECT;
    MISMATCH_FIXED_ARRAY_SIZE;
    INVALID_TAGGED_UNION;
    INVALID_TYPE_IN_TAGGED_UNION;
}

// Parse_State means the next expected thing.
Parse_State :: enum u8 {
    UNINIT;
    KEY;
    COLON;
    VALUE;
    COMMA_OR_END;
}

parse_json :: (s: string) -> Json_Any, Json_Result {
    lexer: Lexer;
    set_input_from_string(*lexer, s);

    root: Json_Any;

    stack: [..]*Json_Any;
    defer array_free(stack);

    array_add(*stack, *root);

    kv: Key_Value;

    state := Parse_State.UNINIT;

    while 1 {
        t := get_token(*lexer);
        if t.type == .ERROR {
            return .{}, .LEXER_ERROR;
        } else if t.type == .EOF {
            break;
        }

        if #complete state == {
        case .UNINIT;
            if t.type == {
            case .L_BRACE;
                root.type = .OBJECT;
                state = .KEY;
            case .L_BRACK;
                root.type = .ARRAY;
                state = .VALUE;
            case;
                return .{}, .EXPECT_OBJECT_OR_ARRAY;
            }
        case .KEY;
            if t.type == {
            case .STRING;
                kv.key = t.str;
                state = .COLON;
            case .R_BRACE;
                parent := peek(stack);
                if parent.type == .OBJECT {
                    pop(*stack);
                    state = .COMMA_OR_END;
                } else {
                    return .{}, .MISMATCH_BRACE;
                }
            case;
                return .{}, .EXPECT_KEY;
            }
        case .COLON;
            if t.type == .COLON {
                state = .VALUE;
            } else {
                return .{}, .EXPECT_COLON;
            }
        case .COMMA_OR_END;
            if t.type == {
            case .COMMA;
                // By supporting empty array and empty struct,
                // trailing comma is also supported.
                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    state = .KEY;
                case .ARRAY;
                    state = .VALUE;
                }
            case .R_BRACE;
                parent := peek(stack);
                if parent.type == .OBJECT {
                    // Poping the stack is enough because the state is
                    // still .COMMA_OR_END
                    pop(*stack);
                } else {
                    return .{}, .MISMATCH_BRACE;
                }
            case .R_BRACK;
                parent := peek(stack);
                if parent.type == .ARRAY {
                    // Same as stated above
                    pop(*stack);
                } else {
                    return .{}, .MISMATCH_BRACKET;
                }
            case;
                return .{}, .EXPECT_COMMA;
            }
        case .VALUE;
            if t.type == {
            case .STRING;
                v := Json_Any.{
                    type = .STRING,
                    str = t.str,
                };
                install_value(stack, *kv, v);
                state = .COMMA_OR_END;

            case .NUMBER;
                v := Json_Any.{
                    type = .NUMBER,
                    number = t.num,
                };

                install_value(stack, *kv, v);
                state = .COMMA_OR_END;

            case .NULL;
                v := Json_Any.{
                    type = .NULL,
                };
                install_value(stack, *kv, v);
                state = .COMMA_OR_END;

            case .TRUE;
                v := Json_Any.{
                    type = .BOOLEAN,
                    boolean = true,
                };
                install_value(stack, *kv, v);
                state = .COMMA_OR_END;

            case .FALSE;
                v := Json_Any.{
                    type = .BOOLEAN,
                    boolean = false,
                };
                install_value(stack, *kv, v);
                state = .COMMA_OR_END;

            case .L_BRACE;
                v := Json_Any.{
                    type = .OBJECT,
                };
                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    kv.value = v;
                    array_add(*parent.object, kv);
                    kv = .{};
                    pv := peek_pointer(parent.object);
                    array_add(*stack, *pv.value);
                case .ARRAY;
                    array_add(*parent.array, v);
                    pv := peek_pointer(parent.array);
                    array_add(*stack, pv);
                }
                state = .KEY;

            case .L_BRACK;
                v := Json_Any.{
                    type = .ARRAY,
                };

                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    kv.value = v;
                    array_add(*parent.object, kv);
                    kv = .{};
                    pv := peek_pointer(parent.object);
                    array_add(*stack, *pv.value);
                case .ARRAY;
                    array_add(*parent.array, v);
                    pv := peek_pointer(parent.array);
                    array_add(*stack, pv);
                }
                state = .VALUE;

            case .R_BRACK;
                // In some case, the value is an empty array, []
                // Pop the current array from the stack
                parent := peek(stack);
                if parent.type == .ARRAY {
                    pop(*stack);
                    state = .COMMA_OR_END;
                } else {
                    return .{}, .MISMATCH_BRACKET;
                }
            case;
                return .{}, .EXPECT_VALUE;
            }
        }
    }

    return root, .OK;
}

parse_json_from_file :: (file_path: string) -> Json_Any, Json_Result {
    s, ok := read_entire_file(file_path);
    if !ok return .{}, .FILE_NOT_FOUND;

    j, r := parse_json(s);
    return j, r;
}

// I feel like I repeat myself too much.
install_value :: (stack: []*Json_Any, kv: *Key_Value, v: Json_Any) {
    parent := peek(stack);
    if parent.type == {
    case .OBJECT;
        kv.value = v;
        array_add(*parent.object, kv.*);
        kv.* = .{};
    case .ARRAY;
        array_add(*parent.array, v);
    }
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

// Return Json_Any object of type NULL if nothing is matched.
json_get :: (value: Json_Any, key: string) -> Json_Any, ok:bool {
    if value.type != .OBJECT return .{}, false;

    for value.object {
        if it.key == key {
            return it.value, true;
        }
    }

    return .{}, false;
}

to_json :: (data: *void, ti: *Type_Info) -> Json_Any, success:bool {
    root: Json_Any;
    success := true;

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
    case .STRUCT;
        t := cast(*Type_Info_Struct)ti;
        if is_tagged_union(t) {
            // Built-in tagged unions is just added in beta 0.2.023
            //
            // It is a hot mess. The tag can either be integer or
            // Type. For the integer tagged union, we have no way to
            // know what the tag means. Therefore, only union tagged
            // with Type is supported.
            //
            // By definition, the tag is located at the zero offset of
            // the struct. For safety measure, checking whether the
            // first member is a type is a good idea.
            //
            // The checking does not pre-check the count, because it
            // should be unnecessary.
            if t.members[0].type != .TYPE {
                report_json_error("Only tagged union tagged with Type is supported");
                return .{}, false;
            }

            ptr := cast(*u8)data;
            ptr += size_of(Type);

            // Casting a Type to *Type_Info is valid but unsafe.
            root, success = to_json(ptr, (cast(**Type_Info)data).*);
        } else {
            root.type = .OBJECT;
            for t.members {
                kv: Key_Value;
                kv.key = copy_string(it.name);

                v, ok := to_json(cast(*u8)data + it.offset_in_bytes, it.type);
                if !ok {
                    return .{}, false;
                }
                kv.value = v;

                array_add(*root.object, kv);
            }
        }

    case .POINTER;
        if data == null {
            root.type = .NULL;
        } else {
            // Find the underlying type of the pointer
            t := cast(*Type_Info_Pointer)ti;
            new_ti := t.pointer_to;

            // What a mess, the nature of this function passes value
            // by pointer, so for a real pointer, the argument
            // accepted is pointer of pointer.
            new_data := (cast(**void)data).*;

            // By the recursive nature of this thing, all pointer
            // would be deference to hell.
            root, success = to_json(new_data, new_ti);
        }
    case .ANY;
        a := cast(*Any)data;
        root, success = to_json(a.value_pointer, a.type);
    }

    return root, success;
}

to_json :: (value: *$T) -> Json_Any, success:bool {
    root, ok := to_json(value, type_info(T));

    return root, ok;
}

from_json :: (json: Json_Any, data: *void, ti: *Type_Info) -> Json_Result {
    if ti.type == {
    case .INTEGER;
        if json.type == .NUMBER {
            from_json_integer(json, data, ti);
        } else {
            report_json_error("Expect NUMBER, get %", json.type);
            return .EXPECT_NUMBER;
        }
    case .FLOAT;
        if json.type == .NUMBER {
            from_json_float(json, data, ti);
        } else {
            report_json_error("Expect NUMBER, get %", json.type);
            return .EXPECT_NUMBER;
        }
    case .BOOL;
        if json.type == .BOOLEAN {
            (cast(*bool)data).* = json.boolean;
        } else {
            report_json_error("Expect BOOLEAN, get %", json.type);
            return .EXPECT_BOOLEAN;
        }
    case .STRING;
        if json.type == .STRING {
            (cast(*string)data).* = copy_string(json.str);
        } else {
            report_json_error("Expect STRING, get %", json.type);
            return .EXPECT_STRING;
        }
    case .ARRAY;
        if json.type == .ARRAY {
            return from_json_array(json, data, ti);
        } else {
            report_json_error("Expect ARRAY, get %", json.type);
            return .EXPECT_ARRAY;
        }
    case .STRUCT;
        t := cast(*Type_Info_Struct)ti;
        if is_tagged_union(t) {
            return from_json_tagged_union(json, data, ti);
        } else if json.type == .OBJECT {
            for t.members {
                v, ok := json_get(json, it.name);
                if !ok continue;

                result := from_json(v, cast(*u8)data + it.offset_in_bytes, it.type);
                if result != .OK return result;
            }
        } else {
            return .EXPECT_OBJECT;
        }
    }

    return .OK;
}

from_json :: (json: Json_Any, obj: *$T) -> Json_Result {
    return from_json(json, obj, type_info(T));
}

repeat :: (sb: *String_Builder, s: string, count: int) {
    n := count;
    while n > 0 {
        append(sb, s);
        n -= 1;
    }
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

hash_string :: (s: string) -> s64 {
    num: s64;
    for s {
        num += it;
    }

    num *= s.count;
    return num;
}

hash_strings :: (arr: []string) -> s64 {
    ret: s64;

    for arr {
        num := hash_string(it);
        ret += num;
    }

    return ret;
}

hash_type_info_struct :: (ti: *Type_Info) -> s64 {
    t := cast(*Type_Info_Struct)ti;
    ret: s64;
    for t.members {
        num := hash_string(it.name);
        ret += num;
    }

    return ret;
}

hash_json_object :: (json: Json_Any) -> s64 {
    ret: s64;
    for json.object {
        num := hash_string(it.key);
        ret += num;
    }
    return ret;
}

is_tagged_union :: (ti: *Type_Info_Struct) -> bool {
    return (ti.textual_flags & (.UNION | .UNION_IS_TAGGED)) == (.UNION | .UNION_IS_TAGGED);
}

from_json_integer :: (value: Json_Any, data: *void, ti: *Type_Info) {
    t := cast(*Type_Info_Integer)ti;

    if t.signed {
        if t.runtime_size == {
        case 1;
            (cast(*s8)data).* = xx value.number;
        case 2;
            (cast(*s16)data).* = xx value.number;
        case 4;
            (cast(*s32)data).* = xx value.number;
        case 8;
            (cast(*s64)data).* = xx value.number;
        }
    } else {
        if t.runtime_size == {
        case 1;
            (cast(*u8)data).* = xx value.number;
        case 2;
            (cast(*u16)data).* = xx value.number;
        case 4;
            (cast(*u32)data).* = xx value.number;
        case 8;
            (cast(*u64)data).* = xx value.number;
        }
    }
}

from_json_float :: (value: Json_Any, data: *void, ti: *Type_Info) {
    if ti.runtime_size == {
    case 4;
        (cast(*float32)data).* = xx value.number;
    case 8;
        (cast(*float64)data).* = value.number;
    }
}

from_json_array :: (value: Json_Any, data: *void, ti: *Type_Info) -> Json_Result {
    t := cast(*Type_Info_Array)ti;
    if #complete t.array_type == {
    case .FIXED;
        if t.array_count != value.array.count return .MISMATCH_FIXED_ARRAY_SIZE;

        // For a fixed array, the array count is hide in the
        // *Type_Info, not in the Array_View_64. Never cast a fixed
        // array into Array_View, that suck.
        for 0..value.array.count - 1 {
            result := from_json(value.array[it], cast(*u8)data + t.element_type.runtime_size * it, t.element_type);
            if result != .OK return result;
        }

    case .VIEW;
        arr := cast(*Array_View_64)data;
        arr.data = alloc(t.element_type.runtime_size * value.array.count);
        memset(arr.data, 0, t.element_type.runtime_size * value.array.count);
        arr.count = value.array.count;

        if t.element_type.type == .STRUCT {
            type_info_struct := cast(*Type_Info_Struct)t.element_type;
            ini := type_info_struct.initializer;
            if ini {
                for 0..value.array.count - 1 {
                    ini(cast(*u8)arr.data + t.element_type.runtime_size * it);
                }
            }
        }

        for 0..value.array.count - 1 {
            result := from_json(value.array[it], cast(*u8)arr.data + t.element_type.runtime_size * it, t.element_type);
            if result != .OK return result;
        }

    case .RESIZABLE;
        arr := cast(*Resizable_Array)data;

        array_reserve(arr, value.array.count, t.element_type.runtime_size);
        memset(arr.data, 0, t.element_type.runtime_size * value.array.count);
        arr.count = value.array.count;

        if t.element_type.type == .STRUCT {
            type_info_struct := cast(*Type_Info_Struct)t.element_type;
            ini := type_info_struct.initializer;
            if ini {
                for 0..value.array.count - 1 {
                    ini(cast(*u8)arr.data + t.element_type.runtime_size * it);
                }
            }
        }

        for 0..value.array.count - 1 {
            result := from_json(value.array[it], cast(*u8)arr.data + t.element_type.runtime_size * it, t.element_type);
            if result != .OK return result;
        }
    }

    return .OK;
}

from_json_tagged_union :: (json: Json_Any, data: *void, ti: *Type_Info) -> Json_Result {
    // Find the accepted types of the union, excluding the first
    // member, all the members are valid type.

    ts := cast(*Type_Info_Struct)ti;

    assert(ts.members.count > 1, "This does not look like a tagged union");
    accepted_type_members: [] Type_Info_Struct_Member;
    accepted_type_members.data =  cast(*void)(cast(*u8)ts.members.data + size_of(Type_Info_Struct_Member));
    accepted_type_members.count = ts.members.count - 1;

    find_type_member :: (members: []Type_Info_Struct_Member, tag: Type_Info_Tag) -> *Type_Info {
        for members {
            if it.type.type == tag {
                return it.type;
            }
        }

        return null;
    }

    find_first_number :: (members: []Type_Info_Struct_Member) -> *Type_Info {
        for members {
            if it.type.type == .INTEGER || it.type.type == .FLOAT {
                return it.type;
            }
        }

        return null;
    }

    // Originally, this pointer cast should be in each switch
    // statement, but the offset_in_bytes should always be 8, so I
    // pull it out.
    //
    // ptr := cast(*u8)data + info.offset_in_bytes;
    real_data := cast(*u8)data + 8;

    if json.type == {
    case .NULL;
        // In some fucking rare case, e.g. lsp has a structure that
        // may take null as a value but not optional, (which we
        // represent it as a pointer). To handle this, we just make
        // the type as void and call it a day.

        info := find_type_member(accepted_type_members, .VOID);
        if info == null {
            report_json_error("Tagged union % does not have VOID type", ts.name);
            return .INVALID_TYPE_IN_TAGGED_UNION;
        }

        ptr := cast(*Type)data;
        ptr.* = void;

    case .BOOLEAN;
        info := find_type_member(accepted_type_members, .BOOL);
        if info == null {
            report_json_error("Tagged union % does not have BOOL type", ts.name);
            return .INVALID_TYPE_IN_TAGGED_UNION;
        }

        ptr := cast(*Type)data;
        ptr.* = bool;
        return from_json(json, real_data, info);

    case .NUMBER;
        // We have no way to know which number type the json
        // represents, just pick the first one.
        info := find_first_number(accepted_type_members);

        if info == null {
            report_json_error("Tagged union % does not have NUMBER type", ts.name);
            return .INVALID_TYPE_IN_TAGGED_UNION;
        }

        ptr := cast(**Type_Info)data;
        ptr.* = info;
        return from_json(json, real_data, info);

    case .STRING;
        info := find_type_member(accepted_type_members, .STRING);
        if info == null {
            report_json_error("Tagged union % does not have STRING type", ts.name);
            return .INVALID_TYPE_IN_TAGGED_UNION;
        }

        ptr := cast(**Type_Info)data;
        ptr.* = info;
        return from_json(json, real_data, info);

    case .OBJECT;
        json_obj_hash := hash_json_object(json);
        info: *Type_Info = null;

        for accepted_type_members {
            if it.type.type == .STRUCT {
                if json_obj_hash == hash_type_info_struct(it.type) {
                    info = it.type;
                    break;
                }
            }
        }

        if info == null {
            report_json_error("Tagged union % does not have matching struct for json .", ts.name, json_print(json));
            return .INVALID_TYPE_IN_TAGGED_UNION;
        }

        // At this point, I am convinced that Type and *Type_Info are
        // equal.
        ptr := cast(**Type_Info)data;
        ptr.* = info;

        return from_json(json, real_data, info);

    case .ARRAY;
        // Find the type of the array object in json array, we assume
        // that all objects are of the same type, which is not always
        // true in real json.

        valid_array_types: [..] *Type_Info;
        for accepted_type_members {
            if it.type.type == .ARRAY {
                array_add(*valid_array_types, it.type);
            }
        }

        if valid_array_types.count == 0 {
            report_json_error("There is no array in this tagged union %", ts.name);
                return .INVALID_TYPE_IN_TAGGED_UNION;
        }

        if json.array.count == 0 {
            // This is a fucking joke. In this case we just find the
            // first matching array in the accepted_types and set its
            // count to zero.

            info: *Type_Info;
            for accepted_type_members {
                if it.type.type == .ARRAY {
                    info = it.type;
                    break;
                }
            }

            ptr := cast(**Type_Info)data;
            ptr.* = info;

        } else {
            first_object := json.array[0];

            if valid_array_types.count == 1 {
                // There is only one array type, check if they match.
                info_arr := cast(*Type_Info_Array)valid_array_types[0].type;
                info_element := info_arr.element_type;


            }

            info: *Type_Info;
            if #complete first_object.type == {
            case .NULL;
                // In this case, I would say fuck you.
                report_json_error("Cannot deduce the corresponding type of an array of null in %", json_print(json));
                return .INVALID_TYPE_IN_TAGGED_UNION;

            case .BOOLEAN;
                info: *Type_Info;
                for valid_array_types {
                    info_arr := cast(*Type_Info_Array)it.type;
                    info_element := info_arr.element_type;
                    if info_element.type == .BOOL {
                        // Yes, its parent array type info
                        info = it;
                        break;
                    }
                }

                if info == null {
                    report_json_error("Expect an array of boolean in tagged union %", ts.name);
                    return .INVALID_TYPE_IN_TAGGED_UNION;
                }

                ptr := cast(**Type_Info)data;
                ptr.* = info;

                return from_json(json, real_data, info);

            case .NUMBER;
                info: *Type_Info;
                for valid_array_types {
                    info_arr := cast(*Type_Info_Array)it.type;
                    info_element := info_arr.element_type;
                    if info_element.type == .INTEGER || info_element.type == .FLOAT {
                        // Yes, its parent array type info
                        info = it;
                        break;
                    }
                }

                if info == null {
                    report_json_error("Expect an array of number in tagged union %", ts.name);
                    return .INVALID_TYPE_IN_TAGGED_UNION;
                }

                ptr := cast(**Type_Info)data;
                ptr.* = info;

                return from_json(json, real_data, info);

            case .STRING;
                info: *Type_Info;
                for valid_array_types {
                    info_arr := cast(*Type_Info_Array)it.type;
                    info_element := info_arr.element_type;
                    if info_element.type == .STRING {
                        // Yes, its parent array type info
                        info = it;
                        break;
                    }
                }

                if info == null {
                    report_json_error("Expect an array of string in tagged union %", ts.name);
                    return .INVALID_TYPE_IN_TAGGED_UNION;
                }

                ptr := cast(**Type_Info)data;
                ptr.* = info;

                return from_json(json, real_data, info);
            case .ARRAY;
                // In the case of array of array in a tagged union, I
                // give up and pick the first array.

                // TODO: Do a recursive checking here, but if your
                // array is nested in a tagged union, that is probably
                // a fucked up structure.
                info: *Type_Info;

                for valid_array_types {
                    info_arr := cast(*Type_Info_Array)it.type;
                    info_element := info_arr.element_type;
                    if info_element.type == .ARRAY {
                        info = it;
                        break;
                    }
                }

                if info == null {
                    report_json_error("Expect an array of string in tagged union %", ts.name);
                    return .INVALID_TYPE_IN_TAGGED_UNION;
                }

                ptr := cast(**Type_Info)data;
                ptr.* = info;

                return from_json(json, real_data, info);

            case .OBJECT;
                info: *Type_Info;

                first_obj_hash := hash_json_object(first_object);
                for valid_array_types {
                    info_arr := cast(*Type_Info_Array)it.type;
                    info_element := info_arr.element_type;
                    if info_element.type == .STRUCT && first_obj_hash == hash_type_info_struct(info_element) {
                        info = it;
                        break;
                    }
                }


                if info == null {
                    report_json_error("No matching array of struct of % in tagged union %", json_print(json), ts.name);
                    return .INVALID_TYPE_IN_TAGGED_UNION;
                }

                ptr := cast(**Type_Info)data;
                ptr.* = info;

                return from_json(json, real_data, info);
            }
        }
    }

    return .OK;
}

report_json_error :: (format: string, arguments: .. Any) {
    #if DEBUG == true {
        log(format, arguments);
    }
} @PrintLike

#scope_module

#import "Basic";
#import "File";
#import "String";
#import,file "lexer.jai";
#import "Memory";
