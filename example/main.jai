Person :: struct {
    name: string;
    age: int;
    jobs: []string;
    is_male: bool;
    height: float;
}

// Basic test
test0 :: () {
    s := read_entire_file("example0.json");
    defer free(s);

    j, r, i := parse_json(s);
    assert(r == .OK, "% %", r, i);
    defer json_free(j);

    {
        push_allocator(temp);
        defer reset_temporary_storage();
        log("Test0:");
        log("json:\n%\n", json_print(j, indent = true));

        p: Person;
        r := from_json(j, *p);
        assert(r == .OK, "%", r);

        log("obj:\n%\n\n", p);
    }
}

// Base array
test1 :: () {
    s := read_entire_file("example1.json");
    defer free(s);

    j, r, i := parse_json(s);
    assert(r == .OK, "% %", r, i);
    defer json_free(j);

    {
        push_allocator(temp);
        defer reset_temporary_storage();
        log("Test 1:");
        log("json:\n%\n", json_print(j));

        p: []Person;
        r := from_json(j, *p);
        assert(r == .OK, "%", r);

        log("obj:\n%\n\n", p);
    }
}

// Struct in struct
Desk :: struct {
    color: string;
    height: float;
    count: int;
    has_drawer: bool;
}

House :: struct {
    desk: Desk;
    area: float;
}

Building :: struct {
    flats: []House;
    level_count: int;
}

test2 :: () {
    s := read_entire_file("example2.json");
    defer free(s);

    j, r, i := parse_json(s);
    assert(r == .OK, "% %", r, i);
    defer json_free(j);

    {
        push_allocator(temp);
        defer reset_temporary_storage();
        log("Test 2:");
        log("json:\n%\n", json_print(j));

        b: Building;
        r := from_json(j, *b);
        assert(r == .OK, "%", r);

        log("obj:\n%\n\n", b);
    }
}

// Anyx
Bar :: struct {
    // This is ugly is not meant to be used outside of binding to
    // other protocol or language
    version: Anyx = .{ optional = true, accepted_types = .[type_info(float), type_info(string)]};
}

test3 :: () {
    s := read_entire_file("example3.json");
    defer free(s);

    j, r, i := parse_json(s);
    assert(r == .OK, "% %", r, i);
    defer json_free(j);

    {
        push_allocator(temp);
        defer reset_temporary_storage();
        log("Test 3:");
        log("json:\n%\n", json_print(j));

        b: []Bar;
        r := from_json(j, *b);
        assert(r == .OK, "%", r);

        for b {
            log("obj:");
            log("%:\t%", it_index, it.version.value);
            log("\n");
        }
    }

}

test4 :: () {
    // Where this would fail because a boolean is not accepted
    s := read_entire_file("example4.json");
    defer free(s);

    j, r, i := parse_json(s);
    assert(r == .OK, "% %", r, i);
    defer json_free(j);

    {
        push_allocator(temp);
        defer reset_temporary_storage();
        log("Test 4:");
        log("json:\n%\n", json_print(j));

        b: Bar;
        r := from_json(j, *b);
        if r != .OK{
            log("%", r);
        }

        log("\n");
    }
}

// Test for optional null
test5 :: () {
    // Where this would fail because a boolean is not accepted
    s := read_entire_file("example5.json");
    defer free(s);

    j, r, i := parse_json(s);
    assert(r == .OK, "% %", r, i);
    defer json_free(j);

    {
        push_allocator(temp);
        defer reset_temporary_storage();
        log("Test 5:");
        log("json:\n%\n", json_print(j));

        b: Bar;
        r := from_json(j, *b);
        if r != .OK{
            log("%", r);
        } else {
            log("%", b);
        }
    }
}

main :: () {
    test0();
    test1();
    test2();
    test3();
    test4();
    test5();
    report_memory_leaks();
}

#load "../module.jai";
#import "Basic"()(MEMORY_DEBUGGER = true);
#import "File";
