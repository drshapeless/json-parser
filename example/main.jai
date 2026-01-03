Person :: struct {
    name: string;
    age: s64;
    jobs: []string;
    is_male: bool;
    height: float32;
}

// Test the primitive types
test0 :: () {
    s := read_entire_file("example0.json");
    defer free(s);

    json, result := parse_json(s);
    defer json_free(json);
    assert(result == .OK, "%", result);
    log("Test 0:");
    log("%", json_print(json,, temp));

    person: Person;

    result = from_json(json, *person,, temp);
    assert(result == .OK, "%", result);
    defer reset_temporary_storage();

    log("%", person);
    log("\n");
}

// Test json array as root
test1 :: () {
    s := read_entire_file("example1.json");
    defer free(s);

    json, result := parse_json(s);
    defer json_free(json);
    assert(result == .OK, "%", result);

    log("Test 1:");
    log("%", json_print(json,, temp));

    // We test all three types of array
    people0: []Person;
    result = from_json(json, *people0,, temp);
    assert(result == .OK, "%", result);

    log("Array View:");
    log("%", people0);

    people1: [2]Person;
    result = from_json(json, *people1,, temp);
    assert(result == .OK, "%", result);

    log("Fixed array:");
    log("%", people1);

    people2: [..]Person;
    result = from_json(json, *people2,, temp);
    assert(result == .OK, "%", result);

    log("Resizable array:");
    log("%", people2);

    defer reset_temporary_storage();

    log("\n");
}

// A complicated structure
test2 :: () {
    Building :: struct {
        flats: []Flat;
        level_count: s64;
    }
    Flat :: struct {
        desk: struct {
            color: string;
            height: float32;
            count: s64;
            has_drawer: bool;
        };
        area: float64;
    }

    s := read_entire_file("example2.json");
    defer free(s);

    json, result := parse_json(s);
    defer json_free(json);
    assert(result == .OK, "%", result);

    log("Test 2:");
    log("%", json_print(json,, temp));

    building: Building;
    result = from_json(json, *building,, temp);
    assert(result == .OK, "%", result);

    log("%", building);

    defer reset_temporary_storage();

    log("\n");
}


// Test tagged union
test3 :: () {
    Json_LSP :: struct {
        version: Version;
    }
    Version :: union tag: Type {
        a: float32;
        b: string;
    }

    s := read_entire_file("example3.json");
    defer free(s);

    json, result := parse_json(s);
    defer json_free(json);
    assert(result == .OK, "%", result);

    log("Test 3:");
    log("%", json_print(json,, temp));

    lsps: []Json_LSP;

    result = from_json(json, *lsps,, temp);
    assert(result == .OK, "%", result);

    for lsps {
        a: Any;
        a.type = cast(*Type_Info)it.version.tag;
        a.value_pointer = cast(*u8)*it + 8;
        log("% %", a.type.type, a);
    }


    defer reset_temporary_storage();

    log("\n");
}

// Test pointer (optional)
test4 :: () {
    Fruit :: struct {
        name: string;
        size: float32;
        color: string;
        origin: *string;
    }
    defer reset_temporary_storage();

    s := read_entire_file("example4.json");
    defer free(s);

    json, result := parse_json(s);
    defer json_free(json);
    assert(result == .OK, "%", result);

    log("Test 4:");
    log("%", json_print(json,, temp));

    fruits: []Fruit;

    result = from_json(json, *fruits,, temp);
    assert(result == .OK, "%", result);

    for fruits {
        log("%", it);
        if it.origin != null {
            log("%", it.origin.*);
        }
    }

    log("\n");
}

test5 :: () {
    Room :: struct {
        area: float32;
        location: string;
        furnitures: Table(string, s32);
    }

    defer reset_temporary_storage();

    s := read_entire_file("example5.json");
    defer free(s);

    json, result := parse_json(s);
    defer json_free(json);
    assert(result == .OK, "%", result);

    log("Test 5:");
    log("%", json_print(json,, temp));

    room: Room;

    result = from_json(json, *room,, temp);
    assert(result == .OK, "%", result);

    for room.furnitures {
        log("%: %", it_index, it);
    }

    j, ok := to_json(*room);
    if !ok {
        log("Failed deserialization");
        return;
    }

    log("%", json_print(j));

    log("\n");
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

#import,file "../module.jai"(DEBUG = true);
#import "Basic"()(MEMORY_DEBUGGER = true);
#import "File";
#import "Hash_Table";
