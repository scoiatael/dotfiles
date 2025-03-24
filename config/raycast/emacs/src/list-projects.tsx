import { closeMainWindow, ActionPanel, Detail, List, Action, Icon, showToast, Toast } from "@raycast/api";

import { useEffect, useState } from "react";
import { exec } from "child_process";
import { promisify } from "util";

const execPromise = promisify(exec);

async function emacs(cmd: string) {
  const { stdout } = await execPromise(`$HOME/.nix-profile/bin/emacsclient -e '(json-encode ${cmd})'`);
  return parseEmacsOutput(stdout);
}

function parseEmacsOutput(output: string) {
  return JSON.parse(JSON.parse(output));
}

async function closeWindow() {
  // Perform any action logic first, then close the window
  await closeMainWindow({ clearRootSearch: true });
}

export default function Command() {
  const [projects, setProjects] = useState<string[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    async function fetchProjects() {
      try {
        const projectList = await emacs("(projectile-open-projects)");
        console.log({ projectList });
        setProjects(projectList);
      } catch (e) {
        setError(e instanceof Error ? e.message : "Unknown error");
        await showToast(Toast.Style.Failure, "Failed to get projects", error);
      } finally {
        setLoading(false);
      }
    }

    fetchProjects();
  }, []);

  return (
    <List isLoading={loading}>
      {error ? (
        <List.EmptyView title="Error" description={error} />
      ) : (
        projects.map((project, index) => (
          <List.Item
            key={index}
            icon={Icon.Folder}
            title={project}
            actions={
              <ActionPanel>
                <Action
                  title={project}
                  onAction={() => closeWindow().then(() => emacs(`(projectile-switch-project-by-name "${project}")`))}
                />
              </ActionPanel>
            }
          />
        ))
      )}
    </List>
  );
}
