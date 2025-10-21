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
    ERROR_DATATYPE_NOT_SUPPORTED;
    ERROR_MISMATCH_FIXED_ARRAY_SIZE;
}

parse_json :: (s: string) -> Json_Any, Json_Result {
    tokens := tokenize(s);

    root: Json_Any;

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
                return null, .EXPECT_OBJECT_OR_ARRAY;
            }
        case .KEY;
            if it.type == .QUOTED_STRING {
                current_key_value.key = unescape(unquote(it.raw_string));
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
                    return null, .ERROR_MISMATCH_BRACE;
                }
            }

        }

    }

    return root, .OK;
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
