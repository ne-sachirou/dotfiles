#!/usr/bin/python
"""asdf module."""

import re
import subprocess
import sys
import typing as t

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


class Version:
    """Version."""

    major: int
    minor: int
    patch: int
    is_stable: bool

    @classmethod
    def version_of(cls, plugin_name: str) -> object:
        """Version of the plugin."""
        version_classes = {
            "clojure": ClojureVersion,
            "elixir": ElixirVersion,
            "erlang": ErlangVersion,
            "golang": Version,
            "haskell": Version,
            "nodejs": NodejsVersion,
            "perl": PerlVersion,
            "python": Version,
            "ruby": Version,
            "rust": RustVersion,
        }
        if plugin_name not in version_classes:
            return Version
        return version_classes[plugin_name]

    @classmethod
    def from_s(cls, str: str) -> t.Optional["Version"]:
        """Create a version from an version expression."""
        version_pattern = r"^(?P<major>\d+)\.(?P<minor>\d+)\.(?P<patch>\d+)$"
        match = re.match(version_pattern, str)
        if not match:
            return None
        return cls(**match.groupdict())

    def __init__(self, major: int, minor: int, patch: int):
        """Initialize."""
        self.major = int(major)
        self.minor = int(minor)
        self.patch = int(patch)
        self.is_stable = True

    def __eq__(self, other: object) -> bool:
        """Equal."""
        if not isinstance(other, Version):
            return False
        return self.__dict__ == other.__dict__

    def __gt__(self, other: object) -> bool:
        """Greater than."""
        if not isinstance(other, Version):
            raise Exception(f"{other} is not a version.")
        if not isinstance(other, self.__class__):
            raise Exception(f"{other} is not a {self.__class__}.")
        if self.is_stable ^ other.is_stable:
            return self.is_stable > other.is_stable  # True > False
        for part in ["major", "minor", "patch"]:
            if self.__dict__[part] != other.__dict__[part]:
                return self.__dict__[part] > other.__dict__[part]
        return False

    def __ne__(self, other: object) -> bool:
        """Not equal."""
        return not self.__eq__(other)

    def __repr__(self) -> str:
        """Representation."""
        return f"{self.major}.{self.minor}.{self.patch}"


class ClojureVersion(Version):
    """Clojure Version."""

    patch2: int

    @classmethod
    def from_s(cls, str: str) -> t.Optional["ClojureVersion"]:
        """Create a version from an version expression."""
        version_pattern = (
            r"^(?P<major>\d+)\.(?P<minor>\d+)\.(?P<patch>\d+)\.(?P<patch2>\d+)$"
        )
        match = re.match(version_pattern, str)
        if not match:
            return None
        return cls(**match.groupdict())

    def __init__(self, major: int, minor: int, patch: int, patch2: int):
        """Initialize."""
        super().__init__(major, minor, patch)
        self.patch2 = int(patch2)

    def __gt__(self, other: object) -> bool:
        """Greater than."""
        super().__gt__(other)
        if self.is_stable ^ other.is_stable:
            return self.is_stable > other.is_stable  # True > False
        for part in ["major", "minor", "patch", "patch2"]:
            if self.__dict__[part] != other.__dict__[part]:
                return self.__dict__[part] > other.__dict__[part]
        return False

    def __repr__(self) -> str:
        """Representation."""
        return f"{self.major}.{self.minor}.{self.patch}.{self.patch2}"


class ElixirVersion(Version):
    """Elixir version."""

    erlang_major: int

    @classmethod
    def from_s(cls, str: str) -> t.Optional["Version"]:
        """Create a version from an version expression."""
        version_pattern = r"^(?P<major>\d+)\.(?P<minor>\d+)\.(?P<patch>\d+)-otp-(?P<erlang_major>\d+)$"
        match = re.match(version_pattern, str)
        if not match:
            return None
        return cls(**match.groupdict())

    def __init__(self, major: int, minor: int, patch: int, erlang_major: int):
        """Initialize."""
        super().__init__(major, minor, patch)
        self.erlang_major = int(erlang_major)

    def __gt__(self, other: object) -> bool:
        """Greater than."""
        super().__gt__(other)
        if self.is_stable ^ other.is_stable:
            return self.is_stable > other.is_stable  # True > False
        for part in ["major", "minor", "patch", "erlang_major"]:
            if self.__dict__[part] != other.__dict__[part]:
                return self.__dict__[part] > other.__dict__[part]
        return False

    def __repr__(self) -> str:
        """Representation."""
        return f"{self.major}.{self.minor}.{self.patch}-otp-{self.erlang_major}"


class ErlangVersion(Version):
    """Erlang version."""

    patch2: int

    @classmethod
    def from_s(cls, str: str) -> t.Optional["ErlangVersion"]:
        """Create a version from an version expression."""
        version_pattern = r"^(?P<major>\d+)\.(?P<minor>\d+)(?:\.(?P<patch>\d+)(?:\.(?P<patch2>\d+))?)?$"
        match = re.match(version_pattern, str)
        if not match:
            return None
        groups = match.groupdict()
        for part in ["patch", "patch2"]:
            if part not in groups:
                groups[part] = 0
        return cls(**groups)

    def __init__(self, major: int, minor: int, patch: int, patch2: int):
        """Initialize."""
        super().__init__(major, minor, patch)
        self.patch2 = int(patch2)

    def __gt__(self, other: object) -> bool:
        """Greater than."""
        super().__gt__(other)
        if self.is_stable ^ other.is_stable:
            return self.is_stable > other.is_stable  # True > False
        for part in ["major", "minor", "patch", "patch2"]:
            if self.__dict__[part] != other.__dict__[part]:
                return self.__dict__[part] > other.__dict__[part]
        return False

    def __repr__(self) -> str:
        """Representation."""
        if self.patch == 0 and self.patch2 == 0:
            return f"{self.major}.{self.minor}"
        if self.patch2 == 0:
            return f"{self.major}.{self.minor}.{self.patch}"
        return f"{self.major}.{self.minor}.{self.patch}.{self.patch2}"


class NodejsVersion(Version):
    """Nodejs version."""

    def __init__(self, major: int, minor: int, patch: int):
        """Initialize."""
        super().__init__(major, minor, patch)
        self.is_stable = self.major % 2 == 0


class PerlVersion(Version):
    """Perl version."""

    def __init__(self, major: int, minor: int, patch: int):
        """Initialize."""
        super().__init__(major, minor, patch)
        self.is_stable = self.minor % 2 == 0


class RustVersion(Version):
    """Rust Version."""

    name: t.Optional[str]

    @classmethod
    def from_s(cls, str: str) -> t.Optional["Version"]:
        """Create a version from an version expression."""
        version_pattern = r"^(?:(?P<major>\d+)\.(?P<minor>\d+)\.(?P<patch>\d+))|(?P<name>stable|nightly)$"
        match = re.match(version_pattern, str)
        if not match:
            return None
        groups = match.groupdict()
        if "name" in groups:
            groups["major"] = groups["minor"] = groups["patch"] = 0
        else:
            groups["name"] = None
        return cls(**groups)

    def __init__(self, major: int, minor: int, patch: int, name: str):
        """Initialize."""
        super().__init__(major, minor, patch)
        self.name = name
        if name == "nightly":
            self.is_stable = False

    def __gt__(self, other: object) -> bool:
        """Greater than."""
        super().__gt__(other)
        if self.name == "stable":
            return not other.name == "stable"
        if self.is_stable ^ other.is_stable:
            return self.is_stable > other.is_stable  # True > False
        for part in ["major", "minor", "patch"]:
            if self.__dict__[part] != other.__dict__[part]:
                return self.__dict__[part] > other.__dict__[part]
        return False

    def __repr__(self) -> str:
        """Representation."""
        if self.name:
            return self.name
        return super().__repr__()


class ToolVersions:
    """Manipulate a .tool-versions file."""

    path: str

    def __init__(self, path: str):
        """.Initialize."""
        self.path = path

    def version(self, plugin_name: str) -> t.Optional[Version]:
        """."""
        raise NotImplementedError()

    def set_version(self, plugin_name: str, version: Version):
        """."""
        raise NotImplementedError()

    def unset_version(self, plugin_name: str):
        """."""
        raise NotImplementedError()


class Asdf:
    """Run asdf."""

    def list_all_plugins(self) -> t.List[str]:
        """asdf plugin list all."""
        process = subprocess.run(
            ["asdf", "plugin", "list", "all"],
            capture_output=True,
            check=True,
            text=True,
        )
        return list(
            map(lambda line: line.partition(" ")[0], process.stdout.splitlines())
        )

    def list_plugins(self) -> t.List[str]:
        """asdf plugin list."""
        process = subprocess.run(
            ["asdf", "plugin", "list"], capture_output=True, check=True, text=True
        )
        return process.stdout.splitlines()

    def add_plugin(self, plugin_name: str):
        """asdf plugin add."""
        subprocess.run(["asdf", "plugin", "add", plugin_name], check=True)

    def list_all_versions(self, plugin_name: str) -> t.List[Version]:
        """asdf list."""
        process = subprocess.run(
            ["asdf", "list", "all", plugin_name],
            capture_output=True,
            check=True,
            text=True,
        )
        version_cls = Version.version_of(plugin_name)
        return list(
            map(lambda line: version_cls.from_s(line), process.stdout.splitlines())
        )

    def list_versions(self, plugin_name: str) -> t.List[Version]:
        """asdf list all."""
        process = subprocess.run(
            ["asdf", "list", plugin_name], capture_output=True, check=True, text=True
        )
        version_cls = Version.version_of(plugin_name)
        return list(
            map(
                lambda line: version_cls.from_s(line.lstrip()),
                process.stdout.splitlines(),
            )
        )

    def install_version(self, plugin_name: str, version: Version):
        """asdf install."""
        if plugin_name == "nodejs":
            subprocess.run(
                [
                    "/bin/bash",
                    "-c",
                    "~/.asdf/plugins/nodejs/bin/import-release-team-keyring",
                ],
                check=True,
            )
        subprocess.run(["asdf", "install", plugin_name, str(version)], check=True)

    def uninstall_version(self, plugin_name: str, version: Version):
        """asdf uninstall."""
        subprocess.run(["asdf", "uninstall", plugin_name, str(version)], check=True)

    def set_global_version(self, plugin_name: str, version: Version):
        """asdf global."""
        subprocess.run(["asdf", "global", plugin_name, str(version)], check=True)

    def set_local_version(self, plugin_name: str, version: Version):
        """asdf local."""
        subprocess.run(["asdf", "local", plugin_name, str(version)], check=True)

    def unset_local_version(self, plugin_name: str):
        """."""
        raise NotImplementedError()

    def reshim(self, plugin_name: str):
        """asdf reshim."""
        subprocess.run(["asdf", "reshim", plugin_name], check=True)


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
