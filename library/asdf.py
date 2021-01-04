#!/usr/bin/python
"""asdf module."""

import sys

from ansible.module_utils.asdf import Asdf, Version
from ansible.module_utils.basic import AnsibleModule

ANSIBLE_METADATA = {
    "metadata_version": "0.0.1",
    "status": ["preview"],
    "supported_by": "community",
}

DOCUMENTATION = """
---
module: asdf
short_description: A module for asdf-vm/asdf version manager
version_added: "2.10"
description:
  - A module for asdf-vm/asdf version manager.
options:
  plugin:
    description:
      - asdf plugin name.
    required: yes
  versions:
    description:
      - List of versions.
    required: yes
  global:
    description:
      - Global version.
    required: no
    default: None
author:
  - "ne_Sachirou (@ne-sachirou)"
"""

EXAMPLES = """
# Install Python plugin and Python.
- asdf:
  plugin: python
  versions:
    - "3.9.1"
    - "3.8.6"
  global: "3.9.1"
"""

RETURN = """
changed:
  description: Is changed or not.
  returned: success
  type: bool
  sample: True
"""


class AsdfModule:
    """asdf module."""

    def __init__(self):
        """Initialize."""
        self.__module = AnsibleModule(
            argument_spec={
                "plugin": {"type": "str", "required": True},
                "versions": {"type": "list", "required": True},
                "global": {"type": "str", "required": False, "default": None},
            },
            supports_check_mode=True,
        )

    def run(self):
        """Run asdf."""
        plugin_name = self.__module.params["plugin"]
        version_cls = Version.version_of(plugin_name)

        def version_from_s(version_s: str) -> Version:
            version = version_cls.from_s(version_s)
            if version is None:
                raise Exception(f"Unknown version {version_s}")
            return version

        try:
            versions = list(
                map(
                    version_from_s,
                    self.__module.params["versions"],
                )
            )
            if self.__module.params["global"] is None:
                global_version = None
            else:
                global_version = version_from_s(self.__module.params["global"])
            asdf = Asdf()
            if self.__module.check_mode:
                if plugin_name not in asdf.list_plugins():
                    self.__module.exit_json(changed=True)
                installed_versions = asdf.list_versions(plugin_name)
                for version in versions:
                    if version not in installed_versions:
                        self.__module.exit_json(changed=True)
                # TODO: global に對應する
                for installed_version in installed_versions:
                    if installed_version not in versions:
                        self.__module.exit_json(changed=True)
                self.__module.exit_json(changed=False)
            changed = False
            if plugin_name not in asdf.list_plugins():
                asdf.add_plugin(plugin_name)
                changed = True
            installed_versions = asdf.list_versions(plugin_name)
            for version in versions:
                if version not in installed_versions:
                    asdf.install_version(plugin_name, version)
                    changed = True
            if global_version is not None:
                asdf.set_global_version(plugin_name, global_version)
                # TODO: changed を適切に變更する
            for installed_version in installed_versions:
                if installed_version not in versions:
                    asdf.uninstall_version(plugin_name, installed_version)
                    changed = True
            self.__module.exit_json(changed=changed)
        except Exception:
            self.__module.fail_json(msg=sys.exc_info()[1])


if __name__ == "__main__":
    AsdfModule().run()
