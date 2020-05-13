
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
