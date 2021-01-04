#!/usr/bin/python
"""Lookup private-values."""

import subprocess

from ansible.errors import AnsibleError
from ansible.plugins.lookup import LookupBase

DOCUMENTATION = """
name: private_values
author: ne_Sachirou (@ne-sachirou)
version_added: "2.10"
short_description: Lookup private-values command.
description:
  - Lookup private-values command.
options:
  _terms:
    description:
      - Arguments of private-values command.
    required: True
notes: []
"""

EXAMPLES = """
- name: Lookup a private-values value.
  debug:
    msg: "{{ lookup('private_values', 'get example_project EXAMPLE_KEY') }}"
- name: Lookup a private-values project path.
  debug:
    msg: "{{ lookup('private_values', 'path example_project') }}"
"""

RETURN = """
_list:
  description:
    - Output of the private-values command.
  type: list
"""


class PrivateValues:
    """Run private-values."""

    def get(self, project: str, key: str) -> str:
        """private-values get."""
        process = subprocess.run(
            ["private-values", "get", f"{project}.{key}"],
            capture_output=True,
            check=True,
            text=True,
        )
        return process.stdout.strip()

    def path(self, project: str) -> str:
        """private-values path."""
        process = subprocess.run(
            ["private-values", "path", project],
            capture_output=True,
            check=True,
            text=True,
        )
        return process.stdout.strip()


class LookupModule(LookupBase):
    """Lookup private-values."""

    def run(self, terms, variables, **kwargs) -> [str]:
        args = terms[0].split(" ")
        if args[0] == "get":
            return [PrivateValues().get(args[1], args[2])]
        elif args[0] == "path":
            return [PrivateValues().path(args[1])]
        raise AnsibleError(f"Unknown command: {args[0]}")
