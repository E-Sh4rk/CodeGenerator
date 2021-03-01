def front_sprite_callback(f, species: int):  # Calculate sprite callback address for a species
    if not 0 <= species < 2**16:
        raise Exception(f'Species {species} out of bounds')
    # JPN
    tAnimId = read(f, 0x082FA374+species-1, 1)  # sMonFrontAnimIdsTable[species - 1]
    target = read(f, 0x085D34E8+4*tAnimId)  # sMonAnimFunctions[tAnimId]
    # US
    tAnimId = read(f, 0x083299ec+species-1, 1)
    target = read(f, 0x0860aa88+4*tAnimId)
    # FR
    tAnimId = read(f, 0x0833155C+species-1, 1)
    target = read(f, 0x0860EE10+4*tAnimId)

    # target = canonicalize(target)
    # print(f'{target:08X}')
    return target
   