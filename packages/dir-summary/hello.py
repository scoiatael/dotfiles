import os
from contextlib import chdir
import argparse
import subprocess
import io

from rich.console import Console
from rich.columns import Columns
from rich.panel import Panel
from rich.table import Table
from rich.layout import Layout
from rich.text import Text

parser = argparse.ArgumentParser(
                    prog='dir-summary',
                    description='Summarize state of given directory')

parser.add_argument('dirname')


def get_content(user):
    """Extract text from user dict."""
    country = user["location"]["country"]
    name = f"{user['name']['first']} {user['name']['last']}"
    return f"[b]{name}[/b]\n[yellow]{country}"


def git():
    commits = subprocess.check_output(["git", "log", "--oneline", "-n10", "--color=always", "--abbrev-commit"]).decode().split("\n")
    table = Table(show_header=False)
    table.add_column("line")
    for commit in commits:
        if commit:
            table.add_row(Text.from_ansi(commit))
    return table

def ls():
    files = subprocess.check_output(["eza", "--oneline", "--color=always", "--icons=always"]).decode().split("\n")
    return Columns([Text.from_ansi(file) for file in files if file])


if __name__ == "__main__":
    console = Console(height=12)
    args = parser.parse_args()
    with chdir(args.dirname):
        try:
            layout = Layout()
            layout.split_row(
                Layout(git(), name="git"),
                Layout(ls(), name="ls")
            )
        except:
            layout = Layout(ls())
        console.print(layout)
