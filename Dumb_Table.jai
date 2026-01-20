// Dumb_Table is a hack to do table_add to a Hash_Table that only has
// data pointer and type_info. It currently only supports string as
// key type. Also, it does not support COUNT_COLLISIONS.

Dumb_Table :: struct {
    count: int;
    allocated: int;
    slots_filled: int;
    allocator: Allocator;
    entries: [] u8;
}

Dumb_Table_Ctx :: struct {
    table:               *Dumb_Table;
    entry_type:          *Type_Info_Struct;
    key_type:            *Type_Info;
    value_type:          *Type_Info;
    hash_function:       #type (string) -> u32;
    compare_function:    #type (string, string) -> bool;
    load_factor_percent: u32;
    refill_removed:      bool;

    SIZE_MIN :: 32;
}

// User should check whether the key_type is string after calling make_dumb_table
make_dumb_table :: (data: *void, ti: *Type_Info) -> Dumb_Table_Ctx {
    // Should I check whether the ti is a table first?
    //
    // At the moment, the caller must make sure the data passed in is
    // a Table.

    ctx: Dumb_Table_Ctx;

    info := ti.(*Type_Info_Struct);
    // This is fragile because when JBlow touch the order of the Table
    // members, all things would fall apart.
    ctx.entry_type = (*info.constant_storage[info.members[4].offset_into_constant_storage]).(**Type_Info_Struct).*;
    // We can also get the key_type and value_type from
    // specified_parameters, but that requires a lot more casting and
    // looks ugly.
    ctx.key_type = ctx.entry_type.members[1].type;
    ctx.value_type = ctx.entry_type.members[2].type;

    ctx.load_factor_percent = (*info.constant_storage[info.specified_parameters[4].offset_into_constant_storage]).(*u32).*;
    ctx.refill_removed = (*info.constant_storage[info.specified_parameters[5].offset_into_constant_storage]).(*bool).*;

    ctx.hash_function = (*info.constant_storage[info.members[6].offset_into_constant_storage]).(**void).*;

    ctx.compare_function = (*info.constant_storage[info.members[7].offset_into_constant_storage]).(**void).*;

    ctx.table = cast(*Dumb_Table)data;

    return ctx;
}

table_add :: (using ctx: Dumb_Table_Ctx, key: *void, value: *void) -> *void {
    Basic.assert(load_factor_percent < 100);

    if (table.slots_filled + 1) * 100 >= table.allocated * load_factor_percent {
        expand(ctx);
    }

    Basic.assert(table.slots_filled < table.allocated);

    // Expand Walk_Table by hand
    mask := cast,trunc(u32)(table.allocated - 1);

    // Since we assume key is always string
    key_str := key.(*string).*;
    hash := hash_function(key_str);

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

    memcpy(entry.(*u8) + entry_type.members[1].offset_in_bytes, key, key_type.runtime_size);

    memcpy(entry.(*u8) + entry_type.members[2].offset_in_bytes, value, value_type.runtime_size);

    return table.entries.data.(*u8) + entry_type.members[2].offset_in_bytes;
}

// for_expansion return *void as it_index and it for key and value
// respectively, in theory, this is mutable.
for_expansion :: (ctx: *Dumb_Table_Ctx, body: Code, flags: For_Flags) #expand {
    #assert(!(flags & .REVERSE));
    #assert(!(flags & .POINTER)); // This is useless as we are going to return a pointer to it_index and it.

    if ctx.table.count {
        for index: 0..ctx.table.entries.count - 1 {
            entry := ctx.table.entries.data + (ctx.entry_type.runtime_size * index);
            hash := entry.(*u32).*;

            if hash < HT.FIRST_VALID_HASH continue;

            `it_index := entry + ctx.entry_type.members[1].offset_in_bytes;
            `it := entry + ctx.entry_type.members[2].offset_in_bytes;

            #insert (remove={entry.(*u32).*=REMOVED_HASH; ctx.table.count -= 1;}) body;
        }
    }
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

    _internal_resize_memory(ctx, new_allocated);

    table.count        = 0;
    table.slots_filled = 0;

    for 0..old_entries.count - 1 {
        entry := old_entries.data + (entry_type.runtime_size * it);
        hash := entry.(*u32).*;

        key_ptr := cast(*u8)entry + entry_type.(*Type_Info_Struct).members[1].offset_in_bytes;

        value_ptr := cast(*u8)entry + entry_type.(*Type_Info_Struct).members[2].offset_in_bytes;

        if hash >= HT.FIRST_VALID_HASH table_add(ctx, key_ptr, value_ptr);
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
    table.entries.data = Basic.alloc(n * entry_type.runtime_size,, table.allocator);
    memset(table.entries.data, 0, n * entry_type.runtime_size);

    for 0..n - 1 {
        entry := table.entries.data + (entry_type.runtime_size * it);

        // This is equal to hash = 0;
        entry.(*u32).* = 0;
    }
}

HT :: #import "Hash_Table";
Basic :: #import "Basic";
