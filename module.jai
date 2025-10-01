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
    union {
        boolean: bool;
        number: float64;
        str: string;
        array: []Json_Value;
        object: *Json_Object;
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

        if c == {
        case #char "{";
            t := Token.{
                type = .OPEN_BRACE,
                raw_string = .{
                    data = s.data + i,
                    count = 1,
                },
            };
            array_add(*tokens, t);
            i += 1;
        case #char "}";
            t := Token.{
                type = .CLOSE_BRACE,
                raw_string = .{
                    data = s.data + i,
                    count = 1,
                },
            };
            array_add(*tokens, t);
            i += 1;
        case #char ":";
            t := Token.{
                type = .COLON,
                raw_string = .{
                    data = s.data + i,
                    count = 1,
                },
            };
            array_add(*tokens, t);
            i += 1;
        case #char ",";
            t := Token.{
                type = .COMMA,
                raw_string = .{
                    data = s.data + i,
                    count = 1,
                },
            };
            array_add(*tokens, t);
            i += 1;
        case #char "\"";
            // Find next double quote
            t := Token.{
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
        case;
            // unquoted string is here
            t := Token.{
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
                if is_space(cc) || cc == #char "," || cc == #char "}" {
                    found = true;
                    t.raw_string.count = new_i - i;
                } else {
                    new_i += 1;
                }
            }
            array_add(*tokens, t);
            t = new_i;
        }
    }

    return tokens;
}

#scope_file
#import "Basic";
#import "String";
