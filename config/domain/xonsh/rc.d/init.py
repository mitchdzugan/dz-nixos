from powerbar import pexport
from xonsh.built_ins import XSH

def main():
    XSH.env["FROM_INIT_PY"] = "Hello govna"
    pexport()

main()
