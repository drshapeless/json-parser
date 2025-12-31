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

    EXPECT_OBJECT_OR_ARRAY;
    EXPECT_KEY;
    EXPECT_COLON;
    EXPECT_COMMA;
    EXPECT_VALUE;
    MISMATCH_BRACE;
    MISMATCH_BRACKET;
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
        if is_tagged_union(ti) {
            // We don't care the accepted value_types of the Tagged_Union
            tg := cast(*Empty_Tagged_Union)data;

            // This is probably unsafe to cast a Type into *Type_Info
            root, success = to_json(tg.bytes.data, cast(*Type_Info)tg.tag);
        } else {
            t := cast(*Type_Info_Struct)ti;
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

            // Dereference it here should be safe.
            new_data := (cast(*u8)data).*;

            root, success = to_json(new_data, new_ti);
        }
    }

    return root, success;
}

to_json :: (value: *$T) -> Json_Any, success:bool {
    root: Json_Any;
    ok := false;
    if T == Any {
        root, ok = to_json(value.value_pointer, value.type);
    } else {
        root, ok = to_json(value, type_info(T));
    }

    return root, ok;
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

hash_type_info_struct :: (ti: *Type_Info_Struct) -> s64 {
    ret: s64;
    for ti.members {
        num := hash_string(it.name);
        ret += num;
    }

    return ret;
}

TAGGED_UNION_HASH :: #run hash_type_info_struct(type_info(Tagged_Union()));

Empty_Tagged_Union :: Tagged_Union();

is_tagged_union :: (ti: *Type_Info) -> bool {
    return hash_type_info_struct(ti) == TAGGED_UNION_HASH;
}

#import "Basic";
#import "File";
#import "String";
#import,file "lexer.jai";
