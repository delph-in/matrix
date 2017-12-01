
def safe_int(param, offset=0):
    """
    Convert the parameter to an int if it represents one, otherwise
    return the original parameter. The offset parameter is added to the
    result of an integer return value (useful for list indices).
    """
    try:
        return int(param) + offset
    except ValueError:
        return param

def get_valid_lines(lines):
    """
    Return all lines that are not empty. Also, strip out byte order marks
    from the first line if it has one.
    """
    from codecs import BOM_UTF8
    if len(lines) > 0 and lines[0].startswith(BOM_UTF8):
        lines[0] = lines[0].strip(BOM_UTF8)
    lines = [l.strip() for l in lines if l.strip() != '']
    return lines
