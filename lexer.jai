Token_Type :: enum u32 {
    ERROR   :: 0;
    L_BRACE :: 1;
    R_BRACE :: 2;
    L_BRACK :: 3;
    R_BRACK :: 4;
    COLON   :: 5;
    COMMA   :: 6;
    STRING  :: 7;
    NULL    :: 8;
    NUMBER  :: 9;
    TRUE    :: 10;
    FALSE   :: 11;
    EOF     :: 12;
}

Token :: struct {
    type: Token_Type;
    l0, c0: s32;
    l1, c1: s32 = -1;

    union {
        str: string;
        num: float64;
    };
}

// These are directly copied from Jai_Lexer
Lexer :: struct {
    current_line_number: s32;
    current_character_index: s32;

    input: string;
    input_cursor: s64;

    reported_error := false;
}

peek_next_character :: (using lexer: *Lexer) -> s16 {
    if input_cursor >= input.count {
        return -1;
    }

    return input[input_cursor];
}

eat_character :: (using lexer: *Lexer) {
    if input[input_cursor] == #char "\n" {
        current_line_number += 1;
        current_character_index = 0;
    }

    input_cursor += 1;
    current_character_index += 1;
}

report_parse_error :: (lexer: *Lexer, format: string, arguments: .. Any) {
    log(format, .. arguments);
    log("... at line %, character %.\n", lexer.current_line_number, lexer.current_character_index);
    lexer.reported_error = true;
} @PrintLike

report_parse_error :: (lexer: *Lexer, token: *Token, format: string, arguments: .. Any) {
    log(format, .. arguments);
    log("... at line %, character %.\n", token.l0, token.c0);
    lexer.reported_error = true;
} @PrintLike

get_token :: (lexer: *Lexer) -> Token {
    while 1 {
        c := peek_next_character(lexer);
        if c < 0 return .{ type = .EOF };

        if is_space(xx c) {
            skip_spaces(lexer);
        } else if is_alpha(xx c) {
            return make_keyword(lexer);
        } else if c == #char "\"" {
            return make_string(lexer);
        } else if c == #char "-" || c == #char "+" || is_digit(xx c) {
            return make_number(lexer);
        } else {
            return make_other(lexer);
        }
    }

    return .{};
}

skip_spaces :: (lexer: *Lexer) {
    while 1 {
        c := peek_next_character(lexer);
        if c < 0 break;

        if is_space(xx c) {
            eat_character(lexer);
        } else {
            return;
        }
    }
}

make_empty_token :: (using lexer: *Lexer) -> Token {
    t := Token.{
        l0 = current_line_number,
        c0 = current_character_index,
    };

    return t;
}

set_end_of_token :: (lexer: *Lexer, token: *Token) {
    token.l1 = lexer.current_line_number;
    token.c1 = lexer.current_character_index;
}

make_keyword :: (using lexer: *Lexer) -> Token {
    t := make_empty_token(lexer);

    start := input_cursor;
    while 1 {
        c := peek_next_character(lexer);
        if c < 0 break;

        if is_alpha(xx c) {
            eat_character(lexer);
        } else {
            break;
        }
    }

    s := slice(input, start, input_cursor - start);

    if s == {
    case "true";
        t.type = .TRUE;
    case "false";
        t.type = .FALSE;
    case "null";
        t.type = .NULL;
    case;
        reported_error = true;
        report_parse_error(lexer, "expect keyword, get %", s);
    }

    set_end_of_token(lexer, *t);

    return t;
}

make_string :: (using lexer: *Lexer) -> Token {
    // Assume the current character is a double quote
    start := input_cursor;
    t := make_empty_token(lexer);

    eat_character(lexer);

    prev_char: u8;
    while 1 {
        c := peek_next_character(lexer);
        if c < 0 break;

        eat_character(lexer);
        if c == #char "\"" && prev_char != #char "\\" {
            break;
        }

        prev_char = xx c;
    }

    s := slice(input, start, input_cursor - start);

    s = unquote(s);

    ss, ok := unescape(s);
    if !ok {
        report_parse_error(lexer, "Error unescaping string");
        reported_error = true;
        return t;
    }

    set_end_of_token(lexer, *t);

    t.type = .STRING;
    t.str = ss;
    return t;
}

unquote :: (s: string) -> string {
    if s.count > 0 {
        if s[0] == #char "\"" && s[s.count - 1] == #char "\"" {
            return slice(s, 1, s.count - 2);
        }
    }

    return s;
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

make_number :: (using lexer: *Lexer) -> Token {
    t := make_empty_token(lexer);

    base : float64 = 1.0;
    first_char := peek_next_character(lexer);
    if first_char == #char "-" {
        base = -1.0;
        eat_character(lexer);
    } else if first_char == #char "+" {
        eat_character(lexer);
    }

    start := input_cursor;

    dot_count := 0;
    while 1 {
        c := peek_next_character(lexer);
        if c < 0 break;

        if is_digit(xx c) {
            eat_character(lexer);
        } else if c == #char "." {
            dot_count += 1;
            if dot_count > 1 {
                report_parse_error(lexer, "number with 2 dots");
                return t;
            }
            eat_character(lexer);
        } else {
            break;
        }
    }

    middle_char := peek_next_character(lexer);
    if middle_char == #char "E" || middle_char == #char "e" {
        eat_character(lexer);
        negative_count := 0;
        while 1 {
            c := peek_next_character(lexer);
            if c < 0 break;

            if is_digit(xx c) {
                eat_character(lexer);
            } else if c == #char "-" {
                negative_count += 1;
                if negative_count > 1 {
                    report_parse_error(lexer, "number with 2 minus in exponent part");
                    return t;
                }
                eat_character(lexer);
            } else {
                break;
            }
        }
    }

    s := slice(input, start, input_cursor - start);

    num, ok := string_to_float64(s);
    if !ok {
        report_parse_error(lexer, "error parsing number, %", s);
        return t;
    }

    set_end_of_token(lexer, *t);

    t.num = num * base;
    t.type = .NUMBER;
    return t;
}

make_other :: (using lexer: *Lexer) -> Token {
    t := make_empty_token(lexer);
    c := peek_next_character(lexer);

    if c == {
    case #char "{";
        t.type = .L_BRACE;
    case #char "}";
        t.type = .R_BRACE;
    case #char "[";
        t.type = .L_BRACK;
    case #char "]";
        t.type = .R_BRACK;
    case #char ":";
        t.type = .COLON;
    case #char ",";
        t.type = .COMMA;
    case;
        report_parse_error(lexer, "expect special character, get %", slice(input, input_cursor, 1));
        reported_error = true;
        return t;
    }
    eat_character(lexer);

    set_end_of_token(lexer, *t);

    return t;
}

set_input_from_string :: (lexer: *Lexer, input: string) {
    lexer.input = input;
    lexer.input_cursor = 0;
    lexer.current_line_number = 1;
    lexer.current_character_index = 1;
}

set_input_from_file :: (lexer: *Lexer, file_path: string) -> bool {
    s, ok := read_entire_file(file_path);
    if !ok return false;

    set_input_from_string(lexer, s);

    return true;
}

#import "Basic";
#import "File";
#import "String";
#import "Unicode";
