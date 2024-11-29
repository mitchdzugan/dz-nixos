from xonsh.built_ins import XSH

def main():
    XSH.env["FROM_INIT_PY_LIB_BAD"] = "Hello govna"

def pexport():
    XSH.env["FROM_INIT_PY_LIB_GOOD"] = "Hello govna"
