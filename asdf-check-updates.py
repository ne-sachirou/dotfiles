#!/usr/bin/env python

import os
import subprocess
import typing as t

from module_utils.asdf import Asdf, Version


def check_updates(asdf: Asdf) -> None:
    for plugin_name in asdf.list_plugins():
        latest_version = max(asdf.list_all_versions(plugin_name))
        installed_versions = asdf.list_versions(plugin_name)
        if installed_versions == []:
            installed_latest_version = None
        else:
            installed_latest_version = max(asdf.list_versions(plugin_name))
        if (
            installed_latest_version is None
            or installed_latest_version < latest_version
        ):
            print(
                f"! {plugin_name} is upgradable to {latest_version} (current is {installed_latest_version})"
            )
        else:
            print(f"{plugin_name} is latest (current is {installed_latest_version})")


if __name__ == "__main__":
    if os.path.exists(".tool-versions"):
        os.remove(".tool-version")
    asdf = Asdf()
    asdf.update()
    check_updates(asdf)
    # subprocess.run(
    #     ["sh", "-c", "asdf plugin list | xargs -t -I{} asdf list {}"], check=True
    # )
    subprocess.run(
        [
            "sh",
            "-c",
            r"""
            locate .tool-version | sort | xargs -I{} sh -c 'echo {} && sort {} | awk '"'"'{print"\t"$0}'"'"' && echo'
            """,
        ],
        check=True,
    )
