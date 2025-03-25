import { exec } from "child_process";
import { promisify } from "util";

const execPromise = promisify(exec);

function parse(output: string) {
  return JSON.parse(JSON.parse(output));
}

async function run(cmd: string) {
  const { stdout } = await execPromise(`$HOME/.nix-profile/bin/emacsclient -e '(json-encode ${cmd})'`);
  return parse(stdout);
}

export async function projectileOpenProjects(): Promise<string[]> {
  return await run("(projectile-open-projects)");
}

export async function projectileSwitchProject(project: string): Promise<void> {
  return await run(`(projectile-switch-project-by-name "${project}")`);
}

export async function workspaceSwitch(project: string): Promise<void> {
  return await run(`(+workspace-switch "${project}")`);
}

export async function workspaceListNames(): Promise<void> {
  return await run(`(+workspace-list-names)`);
}

export async function workspaceKill(name: string): Promise<void> {
  return await run(`(+workspace/kill "${name}")`);
}

export async function projectileRelevantKnownProjects(): Promise<string[]> {
  return await run("(projectile-relevant-known-projects)");
}
