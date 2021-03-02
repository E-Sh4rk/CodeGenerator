# Credits to Merrp

def read(f, addr: int, size: int = 4, signed: bool = False):
    f.seek(addr & 0xffffff)
    b = f.read(size)
    i = int.from_bytes(b, 'little', signed=signed)
    # if size == 4:
    #     print(f'{i:08X}')
    # elif size == 2:
    #     print(f'{i:04X}')
    # elif size == 1:
    #     print(f'{i:02X}')
    return i

def canonicalize(addr: int):  # Un-mirror an address
    if addr & 0xf0000000:  # Invalid
        return addr
    if 0x02000000 <= addr < 0x03000000:
        return addr & 0x0203ffff
    else:
        return addr
        raise NotImplementedError(f'Address {addr:08X} not implemented.')

def front_sprite_callback(f, species: int):  # Calculate sprite callback address for a species
    if not 0 <= species < 2**16:
        raise Exception(f'Species {species} out of bounds')
    # JPN
    # tAnimId = read(f, 0x082FA374+species-1, 1)  # sMonFrontAnimIdsTable[species - 1]
    # target = read(f, 0x085D34E8+4*tAnimId)  # sMonAnimFunctions[tAnimId]
    # US
    # tAnimId = read(f, 0x083299ec+species-1, 1)
    # target = read(f, 0x0860aa88+4*tAnimId)
    # FR
    tAnimId = read(f, 0x0833155C+species-1, 1)
    target = read(f, 0x0860EE10+4*tAnimId)

    # target = canonicalize(target)
    # print(f'{target:08X}')
    return target
   
def list_front_sprite_callbacks(f):
    low = 0x02000000
    high = 0x03000000
    print('Species Nat Addr EVS')
    l = []
    for species in range(2**16):
        target = front_sprite_callback(f, species)
        target2 = canonicalize(target)
        if (low <= target < high) and 0x202f000 <= target2 < 0x2031000:#and target != 0x02020000 and target != 0x02fe0600:
            hp = species & 0xff
            at = (species >> 8) & 0xff
            l.append((species, target2, hp, at))
            print(f'{species:04x} = {target2:08x} ({hp:03d} HP {at:03d} AT)')
    # l.sort(key=lambda tup: tup[2]+tup[3])
    # for species, target, hp, at in l:
    #     print(f'{species:04x} = {target:08x} ({hp:03d} HP {at:03d} AT)')

f = open("../../Pkmn/Pokemon - Version Emeraude (FR).gba", "rb")
list_front_sprite_callbacks(f)