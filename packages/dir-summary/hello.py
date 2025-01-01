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


def git_log():
    commits = subprocess.check_output(["git", "log", "--oneline", "-n10", "--color=always", "--abbrev-commit"]).decode().split("\n")
    table = Table(show_header=False)
    table.add_column("line")
    for commit in commits:
        if commit:
            table.add_row(Text.from_ansi(commit))
    return table

def git_status():
    cmd = ["env",
    "GIT_STATUS_COLOR=always",
    "git",
    "--config-env=status.color=GIT_STATUS_COLOR",
    "status",
    "--short"]
    status = subprocess.check_output(cmd).decode()
    return Panel(Text.from_ansi(status))

def ls():
    files = subprocess.check_output(["eza", "--oneline", "--color=always", "--icons=always"]).decode().split("\n")
    return Columns([Text.from_ansi(file + "  ") for file in files if file])


def main():
    console = Console(force_terminal=True, file=io.StringIO())
    args = parser.parse_args()
    with chdir(args.dirname):
        table = Panel(os.getcwd())
        console.print(table)

        if os.path.isdir(".git"):
            console.print(git_status())
            layout = Layout()
            layout.split_row(
                Layout(git_log(), name="git"),
                Layout(ls(), name="ls")
            )
            layout["git"].size = 60
        else:
            layout = Layout(ls())
        console.print(layout)
    buf = console.file.getvalue()
    nonempty = [l for l in buf.split("\n") if l.strip()]
    print("\n".join(nonempty))

if __name__ == "__main__":
    main()
