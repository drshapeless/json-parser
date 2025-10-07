// This json parser does not support json with comment and trailing comma.

Json_Type :: enum u8 {
    NULL;
    BOOLEAN;
    NUMBER;
    STRING;
    ARRAY;
    OBJECT;
}

Json_Value :: struct {
    type: Json_Type;
    raw_string: string;
    union {
        boolean: bool;
        number: float64;
        str: string;
        array: [..]*Json_Value;
        object: Json_Object;
    }
}

Key_Value :: struct {
    key: string;
    value: *Json_Value;
}

Json_Object :: struct {
    contents: [..]Key_Value;
}

json_print :: (sb: *String_Builder, value: *Json_Value) {
    if value == null return;

    // This function uses recursion, I am not sure whether it is a
    // good idea.
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
        for value.array {
            json_print(sb, it);
            if it_index != value.array.count - 1 {
                append(sb, ",");
            }
        }
        append(sb, "]");
    case .OBJECT;
        append(sb, "{");
        for value.object.contents {
            append(sb, "\"");
            append(sb, it.key);
            append(sb, "\"");
            append(sb, ":");
            json_print(sb, it.value);
            if it_index != value.object.contents.count - 1 {
                append(sb, ",");
            }
        }
        append(sb, "}");
    }
}

// Return null if nothing is matched.
json_get :: (value: *Json_Value, key: string) -> *Json_Value {
    if value.type != .OBJECT return null;

    for value.object.contents {
        if it.key == key {
            return it.value;
        }
    }

    return null;
}

// TODO: WIP
to_json :: (obj: $T) -> *Json_Value, ok:bool {
    ti := type_info(T);

    root := New(Json_Value);

    if ti.type == {
    case .STRUCT;
        ;
    case .ARRAY;
        ;
    case .POINTER;
        ;
    }

    return root, true;
}

Parse_Result :: enum {
    OK;
    EXPECT_OBJECT_OR_ARRAY;
    EXPECT_KEY;
    EXPECT_COLON;
    EXPECT_VALUE;
    EXPECT_COMMA;
    ERROR_PARSING_NUMBER;
    ERROR_MISMATCH_BRACE;
    ERROR_MISMATCH_SQUARE_BRACKET;
}

parse_json :: (s: string) -> *Json_Value, Parse_Result {
    tokens := tokenize(s);

    root := New(Json_Value);

    stack: [..]*Json_Value;
    defer array_free(stack);

    array_add(*stack, root);

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
                root.raw_string = it.raw_string;
                root.object = .{};
                state = .KEY;
            } else if it.type == .OPEN_SQUARE_BRACKET {
                root.type = .ARRAY;
                root.raw_string = it.raw_string;
                // root.array = .{};
                state = .VALUE;
            } else {
                // format error
                return null, .EXPECT_OBJECT_OR_ARRAY;
            }
        case .KEY;
            if it.type == .QUOTED_STRING {
                current_key_value.key = unquote(it.raw_string);
                state = .COLON;
            } else {
                return null, .EXPECT_KEY;
            }
        case .COLON;
            if it.type == .COLON {
                state = .VALUE;
            } else {
                return null, .EXPECT_COLON;
            }
        case .COMMA_OR_END;
            if it.type == {
            case .COMMA;
                // TODO: Check if it is a trailing comma, now it does
                // not support trailing comma.
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
                    parent.raw_string.count = it.raw_string.data - parent.raw_string.data + 1;
                    pop(*stack);

                    // Just poping the stack is enough, because the
                    // state is still .COMMA_OR_END when we finish a
                    // value.
                } else {
                    return null, .ERROR_MISMATCH_BRACE;
                }
            case .CLOSE_SQUARE_BRACKET;
                parent := peek(stack);
                if parent.type == .ARRAY {
                    parent.raw_string.count = it.raw_string.data - parent.raw_string.data + 1;
                    pop(*stack);
                } else {
                    return null, .ERROR_MISMATCH_SQUARE_BRACKET;
                }
            case;
                return null, .EXPECT_COMMA;
            } ;
        case .VALUE;
            if it.type == {
            case .QUOTED_STRING;
                v := New(Json_Value);
                v.type = .STRING;
                v.raw_string = it.raw_string;
                v.str = unquote(it.raw_string);

                parent := peek(stack);
                if parent.type == .OBJECT {
                    current_key_value.value = v;
                    array_add(*parent.object.contents, current_key_value);
                    current_key_value = .{};
                } else if parent.type == .ARRAY {
                    array_add(*parent.array, v);
                } else {
                    assert(false);
                }

                state = .COMMA_OR_END;

            case .UNQUOTED_STRING;
                v, ok := parse_non_string(it.raw_string);
                if !ok {
                    return null, .ERROR_PARSING_NUMBER;
                }

                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    current_key_value.value = v;
                    array_add(*parent.object.contents, current_key_value);
                    current_key_value = .{};
                case .ARRAY;
                    array_add(*parent.array, v);
                case;
                    assert(false);
                }

                state = .COMMA_OR_END;

            case .OPEN_BRACE;
                v := New(Json_Value);
                v.type = .OBJECT;
                v.raw_string = it.raw_string;
                v.object = .{};

                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    current_key_value.value = v;
                    array_add(*parent.object.contents, current_key_value);
                    current_key_value = .{};
                case .ARRAY;
                    array_add(*parent.array, v);
                case;
                    assert(false);
                }

                array_add(*stack, v);
                state = .KEY;

            case .OPEN_SQUARE_BRACKET;
                v := New(Json_Value);
                v.type = .ARRAY;
                v.raw_string = it.raw_string;

                parent := peek(stack);
                if parent.type == {
                case .OBJECT;
                    current_key_value.value = v;
                    array_add(*parent.object.contents, current_key_value);
                    current_key_value = .{};
                case .ARRAY;
                    array_add(*parent.array, v);
                case;
                    assert(false);
                }

                array_add(*stack, v);
                state = .VALUE;

            case;
                return null, .EXPECT_VALUE;
            }
        }
    }

    return root, .OK;
}

free_json :: (json: *Json_Value) {
    if json.type == {
    case .ARRAY;
        for json.array {
            free_json(it);
        }
        array_free(json.array);
    case .OBJECT;
        for json.object.contents {
            free_json(it.value);
        }
        array_free(json.object.contents);
    }

    free(json);
}

#scope_file
Token_Type :: enum {
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
    defer array_free(tokens);
    i := 0;
    while i < s.count {
        c := s[i];
        if is_space(c) {
            i += 1;
            continue;
        }

        t: Token;
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
            t = .{
                type = .QUOTED_STRING,
                raw_string = .{
                    data = s.data + i,
                },
            };
            new_i := i + 1;
            found := false;
            while !found {
                if s[new_i] == #char "\"" && s[new_i - 1] != "\\" {
                    found = true;
                    t.raw_string.count = new_i - i + 1;
                }
                new_i += 1;
            }
            array_add(*tokens, t);
            i = new_i;
            continue;
        case;
            // unquoted string is here
            t = .{
                type = .UNQUOTED_STRING,
                raw_string = .{
                    data = s.data + i,
                },
            };

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
            array_add(*tokens, t);
            i = new_i;
            continue;
        }

        t.raw_string = .{
            data = s.data + i,
            count = 1,
        };
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



unquote :: (s: string) -> string {
    if s.count > 0 {
        if s[0] == #char "\"" && s[s.count - 1] == #char "\"" {
            return slice(s, 1, s.count - 2);
        }
    }

    return s;
}

parse_non_string :: (s: string) -> *Json_Value, ok:bool {
    v := New(Json_Value);
    if s ==  {
    case "true";
        v.type = .BOOLEAN;
        v.raw_string = s;
        v.boolean = true;
    case "false";
        v.type = .BOOLEAN;
        v.raw_string = s;
        v.boolean = false;
    case "null";
        v.type = .NULL;
        v.raw_string = s;
    case;
        // Should be number.
        num, ok := string_to_float64(s);
        if !ok {
            free(v);
            return null, false;
        }

        v.type = .NUMBER;
        v.raw_string = s;
        v.number = num;
    }

    return v, true;
}

#import "Basic";
#import "String";
