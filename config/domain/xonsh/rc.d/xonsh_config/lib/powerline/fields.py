import functools
import os
import re
import typing as tp
from pathlib import Path

import xonsh.tools as xt
from xonsh.built_ins import XSH
from .colors import Colors, get_contrast_color

env = XSH.env
FLDS = env["PROMPT_FIELDS"]
FLDS["time_format"] = "%I:%M:%S%p"


class RichField(tp.NamedTuple):
    value: str
    fg: str
    sep: "str|None" = None


def add_pl_field(fn):
    """a decorator that add function wrappers for $PROMPT_FIELDS `fn` and `fn__pl_colors`"""
    fld_name = fn.__name__

    @functools.wraps(fn)
    def wrapped():
        result = fn()
        # if it is tuple then it contains color info as well
        if isinstance(result, RichField):
            value = result.value
            add_pl_colors(fld_name, result.fg)
            if result.sep:
                add_pl_sep(fld_name, result.sep)
        else:
            value = result
        return value

    FLDS[fld_name] = wrapped
    return wrapped


def add_field(fn):
    FLDS[fn.__name__] = fn
    return fn


def add_pl_colors(name: str, bg: str, color: "str|None" = None):
    """for the prompt field set the background and foreground color"""
    if bg:
        color = color or get_contrast_color(bg.lstrip("#"))
        colors = (color, bg)
    else:
        colors = ("", "")  # no need to add colors. eg. prompt_end
    FLDS[f"{name}__pl_colors"] = colors


def add_pl_sep(name: str, sep: str):
    """for the prompt field thin separator"""
    FLDS[f"{name}__pl_sep"] = sep


def set__pl_defaults():
    """add colors/sep for the default prompt-fields"""
    for defs in [
        ("user", Colors.SAND),
        ("hostname", Colors.BLUE),
        ("localtime", Colors.SAND),
        ("env_name", Colors.EMERALD),
        ("current_job", Colors.ROSE),
        ("prompt_end", ""),
        ("cwd", Colors.CYAN),
        ("env_name", Colors.EMERALD),
        ("full_env_name", Colors.EMERALD),
    ]:
        add_pl_colors(*defs)

    for fld, sep in [
        ("cwd", os.sep),
    ]:
        add_pl_sep(fld, sep)


@functools.lru_cache
def poetry_env_naming():
    # if using poetry's venv naming scheme like <venv-name>-<hash>-py<ver>
    return re.compile(r"\(?(?P<name>\S+)-\w+-+py(?P<version>[\d.]+)\)?")


@add_pl_field
def full_env_name():
    """When the `env_name` is
    - `.venv` show the name of the parent folder
    - contains `-py3.*` (when it is poetry created) shows the project name part alone
    """
    env_name: str = FLDS.pick("env_name")
    if not env_name:
        return

    sep = "\0"
    if venv := env.get("VIRTUAL_ENV"):
        venv_path = Path(venv)
        if venv_path.name == ".venv":
            env_name = venv_path.parent.name

        if match := poetry_env_naming().match(venv_path.name):
            name, version = match.groups()
            env_name = sep.join([name, version])

    return RichField(env_name, Colors.EMERALD, sep)


def _background_jobs():
    num = len([task for task in XSH.all_jobs.values() if task.get("bg")])
    if num:
        return f"💼{num}"


@add_pl_field
def background_jobs():
    """Show number of background jobs"""
    jobs = _background_jobs()
    if jobs:
        return RichField(_background_jobs(), Colors.SERENE)


def deep_get(dictionary, *keys) -> tp.Optional[tp.Any]:
    """
    >>> deep_get({"1": {"10": {"100": 200}}}, "1", "10", "100")
    200
    """
    dic = dictionary
    for ky in keys:
        if dic is None:
            return None
        dic = dic.get(ky)
    return dic


# @add_pl_field
# def py_pkg_info():
#     """Show package name and version if current directory has setup.py or pyproject.toml"""
#     py_logo = "\ue73c"  #  - python logo font
#     import tomlkit
#
#     # todo
#     proj = tomlkit.parse(proj_file.read_text())
#
#     proj_name = deep_get(proj, "tool", "poetry", "name")
#     proj_version = deep_get(proj, "tool", "poetry", "version")
#
#     return f"{py_logo} {proj_name}-{proj_version}"
#
#
# @add_pl_field
# def os_icon():
#     """Show logo from nerd-fonts for current distro"""
#
#     # todo
#     return None


@add_pl_field
def user_at_host():
    val = FLDS["user"] + "✸" + FLDS["hostname"]
    return RichField(val, Colors.VIOLET)


@add_pl_field
def ret_code():
    if XSH.history.rtns:
        return_code = XSH.history.rtns[-1]
        if return_code != 0:
            return RichField(f"[{return_code}]", Colors.RED)


@add_field
def prompt_end():
    """prompt end symbol with color red if last job failed"""
    prompt = "#" if xt.is_superuser() else "❯"
    color = Colors.WHITE
    if XSH.history and XSH.history.rtns:
        return_code = XSH.history.rtns[-1]
        if return_code != 0:
            color = Colors.RED
    return "{%s}%s{RESET} " % (color, prompt)


@add_pl_field
def gitstatus():
    """powerline version of gitstatus prompt-field from Xonsh"""
    from xonsh.prompt import gitstatus as gs

    try:
        gs_dir = FLDS.pick(gs.repo_path)
    except Exception:
        gs_dir = None

    if not gs_dir:
        return None

    fields = set(gs.gitstatus.get_frags(XSH.env))

    def strip_color(symbol: str) -> str:
        if "}" in symbol:
            symbol = symbol.split("}")[1]
        return symbol

    def gather_group(*flds):
        for fld in flds:
            if fld.name.replace("gitstatus", "", 1) not in fields:
                continue
            val = FLDS.pick(fld)

            if not val:
                continue

            yield strip_color(val.prefix) + format(val.value) + strip_color(val.suffix)

    def get_parts():
        for grp in (
            (gs.branch,),
            (gs.operations,),
            (gs.ahead, gs.behind),
            (gs.staged, gs.conflicts),
            (gs.changed, gs.deleted),
            (gs.untracked, gs.stash_count),
            (gs.lines_added, gs.lines_removed),
        ):  # each group appears inside a separator
            val = "".join(gather_group(*grp))
            if val:
                yield val

    def get_color():
        if FLDS.pick(gs.conflicts):
            return Colors.ORANGE
        if any(map(lambda x: FLDS.pick(x), [gs.changed, gs.deleted, gs.untracked])):
            return Colors.PINK

        return Colors.GREEN  # clean

    sep = "\0"
    return RichField(sep.join(get_parts()), get_color(), sep)
