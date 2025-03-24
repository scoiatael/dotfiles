import { closeMainWindow, ActionPanel, Detail, List, Action, Icon, showToast, Toast } from "@raycast/api";
import { usePromise } from "@raycast/utils";

import * as emacs from "./emacs";

async function closeWindow() {
  await closeMainWindow({ clearRootSearch: true });
}

export default function Command() {
  const { isLoading, data, error } = usePromise(async () => {
    return await emacs.projectileRelevantKnownProjects();
  });

  return (
    <List isLoading={isLoading}>
      {error ? (
        <List.EmptyView title="Error" description={error} />
      ) : (
        data?.map((project, index) => (
          <List.Item
            key={index}
            icon={Icon.Folder}
            title={project}
            actions={
              <ActionPanel>
                <Action
                  title={project}
                  onAction={() => closeWindow().then(() => emacs.projectileSwitchProject(project))}
                />
              </ActionPanel>
            }
          />
        ))
      )}
    </List>
  );
}
