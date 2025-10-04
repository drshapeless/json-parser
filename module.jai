// This json parser does not support json with comment.

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
    parent: *Json_Object;
    union {
        boolean: bool;
        number: float64;
        str: string;
        array: []Json_Value;
        object: Json_Object;
    }
}

Key_Value :: struct {
    key: string;
    value: Json_Value;
}

Json_Object :: struct {
    contents: [..]Key_Value;
}

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

parse_json :: (s: string) -> Json_Value, ok:bool {
    tokens := tokenize(s);

    root: Json_Value;
    root.type = .NULL;

    current_key := "";
    current_value: Json_Value;
    current_array: [..] Json_Value;
    current_object: *Json_Object;

    is_key := false;
    is_object := false;
    is_array := false;

    for tokens {
        if root.type == .NULL {
            if it.type == {
            case .OPEN_BRACE;
                root.type = .OBJECT;
                root.raw_string = it.raw_string;
                is_object = true;
                is_key = true;
                current_object = *root.object;
            case .OPEN_SQUARE_BRACKET;
                root.type = .ARRAY;
                root.raw_string = it.raw_string;
                is_array = true;
            case;
                log("The root node is neither an object or an array.");
                return .{}, false;
            }
        } else if is_key {
            if it.type != .QUOTED_STRING {
                log("Format error at pos %", it.raw_string.data - s.data);
                return .{}, false;
            }

            current_key = unquote(it.raw_string);
            is_key = false;
        } else {
            if #complete it.type == {
            case .OPEN_BRACE;
                ;
            case .CLOSE_BRACE;
                ;
            case .COLON;
                ;
            case .COMMA;
                ;
            case .QUOTED_STRING;
                current_value.type = .STRING;
                current_value.raw_string = it.raw_string;
                current_value.str = unquote(it.raw_string);
                current_value.parent = current_object;
                if is_object {
                    kv := Key_Value.{
                        key = current_key,
                        value = current_value,
                    };
                    array_add(*current_object.contents, kv);

                    current_key = .{};
                    current_value = .{};
                    is_key = true;
                } else if is_array {
                    array_add(*current_array, current_value);
                    current_value = .{};
                } else {
                    assert(false, "Something is very wrong!");
                }
            case .UNQUOTED_STRING;
                current_value = parse_non_string(it.raw_string);
                if is_object {
                    kv := Key_Value.{
                        key = current_key,
                        value = current_value,
                    };
                    array_add(*current_object.contents, kv);

                    current_key = .{};
                    current_value = .{};
                    is_key = true;
                } else if is_array {
                    array_add(*current_array, current_value);
                    current_value = .{};
                } else {
                    assert(false, "Something is very wrong!");
                }
            case .OPEN_SQUARE_BRACKET;
                ;
            case .CLOSE_SQUARE_BRACKET;
                ;
            }
        }
    }

    return root, true;
}

unquote :: (s: string) -> string {
    if s.count > 0 {
        if s[0] == #char "\"" && s[s.count - 1] == #char "\"" {
            return slice(s, 1, s.count - 2);
        }
    }

    return s;
}

parse_non_string :: (s: string) -> Json_Value {
    if s ==  {
    case "true";
        return .{
            type = .BOOLEAN,
            raw_string = s,
            boolean = true,
        };
    case "false";
        return .{
            type = .BOOLEAN,
            raw_string = s,
            boolean = false,
        };
    case "null";
        return .{
            type = .NULL,
            raw_string = s,
        };
    case;
    // Should be number.
        num, ok := string_to_float64(s);
        if !ok {
            log("Format error, % is not a valid number", s);
            return .{};
        }

        return .{
            type = .NUMBER,
            raw_string = s,
            number = num,
        };
    }
}

#scope_file
#import "Basic";
#import "String";
