import { Detail } from "@raycast/api";
import { usePromise } from "@raycast/utils";

import * as emacs from "./emacs";

export default function Command() {
  const { isLoading, data } = usePromise(async () => {
    return await emacs.projectileOpenProjects();
  }, []);

  let mrkdown = `You have ${data?.length} open projects`;
  data?.forEach((p) => (mrkdown += `\n - ${p}`));
  return <Detail isLoading={isLoading} markdown={mrkdown} />;
}
