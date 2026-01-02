Dumb_Table :: struct {
    count: int;
    allocated: int;
    slots_filled: int;
    allocator: Allocator;
    entries: [] u8;
}

Dumb_Table_Ctx :: struct {
    table: *Dumb_Table_Data;
    entry_type: *Type_Info;
    load_factor_percent: u32;
    refill_removed: bool;
    SIZE_MIN :: 32;
}

table_add :: (using ctx: Dumb_Table_Ctx, key: *void, value: *void) -> *void {
    assert(load_factor_percent < 100);

    if (table.slots_filled + 1) * 100 >= table.allocated * load_factor_percent {
        expand(ctx);
    }

    assert(table.slots_filled < table.allocated);

    // Expand Walk_Table by hand
    mask := cast,trunc(u32)(table.allocated - 1);

    // Since we assume key is always string
    key_str := key.(*string).*;
    hash := Hash.get_hash(key_str);
    if hash < HT.FIRST_VALID_HASH {
        hash += HT.FIRST_VALID_HASH;
    }

    index := hash & mask;

    probe_increment : u32 = 1;

    while 1 {
        entry := table.entries.data + (entry_type.runtime_size * index);
        entry_hash := entry.(*u32).*;
        if entry_hash == 0 break;

        if refill_removed {
            if entry_hash == HT.REMOVED_HASH {
                table.slots_filled -= 1;
                break;
            }
        }

        index = (index + probe_increment) & mask;
        probe_increment += 1;
    }

    table.count        += 1;
    table.slots_filled += 1;

    entry := table.entries.data + (entry_type.runtime_size * index);
    entry.(*u32).* = hash;

    key_type := entry_type.(*Type_Info_Struct).members[1];
    memcpy(table.entries.data.(*u8) + key_type.offset_in_bytes, key_type.runtime_size);

    value_type := entry_type.(*Type_Info_Struct).members[2];
    memcpy(table.entries.data.(*u8) + value_type.offset_in_bytes, value_type.runtime_size);

    return table.entries.data.(*u8) + value_type.offset_in_bytes;
}

expand :: (using ctx: Dumb_Table_Ctx) {
    old_entries := table.entries;

    new_allocated: s64 = ---;

    if (table.count * 2 + 1) * 100 < table.allocated * load_factor_percent {
        new_allocated = table.allocated;
    } else {
        new_allocated = table.allocated * 2;
    }

    if new_allocated < SIZE_MIN  new_allocated = SIZE_MIN;

    _internal_resize_memory(table, new_allocated, entry_type);

    table.count        = 0;
    table.slots_filled = 0;

    for 0..old_entries.count - 1 {
        entry := old_entries.data + (entry_type.runtime_size * it);
        hash := entry.(*u32).*;

        ptr := cast(*u8)entry + entry_type.(*Type_Info_Struct).members[1].offset_in_bytes;

        if hash >= HT.FIRST_VALID_HASH table_add(table, ptr, entry_type);
    }

    Basic.free(old_entries.data,, table.allocator);
}

#scope_file
_internal_resize_memory :: (using ctx: Dumb_Table_Ctx, slots_to_allocate: s64 = 0) {
    if slots_to_allocate == 0 slots_to_allocate = SIZE_MIN;
    n := HT.next_power_of_two(slots_to_allocate);
    // next_power_of_two from Hash_Table can be reused
    table.allocated = n;

    // Originally it calls NewArray, but we don't have the type.
    table.entries.count = n;
    table.entries.data = Basic.alloc(n * table.entry_type.runtime_size,, table.allocator);

    for 0..n - 1 {
        entry := table.entries.data + (entry_type.runtime_size * it);

        // This is equal to hash = 0;
        entry.(*u32).* = 0;
    }
}

HT :: #import "Hash_Table";
Basic :: #import "Basic";
Hash :: #import "Hash";
