"""Minimal protobuf wire-format walker for Shimadzu LCD parameter streams.

The streams start with a 64-byte ASCII name padded with NULs, then a bare
protobuf message with no schema available, so this walks the wire format and
reports (field path, wire type, value) without interpreting field numbers.
"""
import struct

def varint(b, i):
    v = 0; s = 0
    while i < len(b):
        x = b[i]; i += 1
        v |= (x & 0x7F) << s
        if not (x & 0x80):
            return v, i
        s += 7
        if s > 63:
            raise ValueError("varint too long")
    raise ValueError("truncated varint")

def as_signed(v):
    return v - (1 << 64) if v >= (1 << 63) else v

def walk(b, path="", depth=0, out=None):
    if out is None: out = []
    i = 0
    while i < len(b):
        try:
            tag, i = varint(b, i)
        except ValueError:
            break
        fn, wt = tag >> 3, tag & 7
        p = f"{path}.{fn}" if path else str(fn)
        try:
            if wt == 0:
                v, i = varint(b, i)
                out.append((p, "varint", v, as_signed(v)))
            elif wt == 1:
                if i + 8 > len(b): break
                raw = b[i:i+8]; i += 8
                out.append((p, "fixed64", struct.unpack("<Q", raw)[0],
                            struct.unpack("<d", raw)[0]))
            elif wt == 2:
                n, i = varint(b, i)
                if i + n > len(b): break
                sub = b[i:i+n]; i += n
                printable = all(32 <= c < 127 for c in sub) and n > 0
                if printable:
                    out.append((p, "string", sub.decode("ascii"), None))
                elif depth < 6 and n > 1:
                    try:
                        walk(sub, p, depth + 1, out)
                    except Exception:
                        out.append((p, "bytes", sub.hex(), None))
                else:
                    out.append((p, "bytes", sub.hex(), None))
            elif wt == 5:
                if i + 4 > len(b): break
                raw = b[i:i+4]; i += 4
                out.append((p, "fixed32", struct.unpack("<I", raw)[0],
                            struct.unpack("<f", raw)[0]))
            else:
                break
        except ValueError:
            break
    return out

def load(raw):
    """Strip the 64-byte ASCII header and walk the message."""
    name = raw[:64].split(b"\x00")[0].decode("ascii", "replace")
    return name, walk(raw[64:])
