{ llm, buildPythonPackage, fetchFromGitHub, setuptools, click, pytestCheckHook
}:

buildPythonPackage {
  pname = "llm-commit";
  version = "1.0.3";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "GNtousakis";
    repo = "llm-commit";
    rev = "a103ef718e7021a505affdbf014ec83fd421b6c1"; # v1.0.3
    hash = "sha256-gJ9yE5W/mqRsg+R9ehaZf5VHXgzR9WHESKjZzhQdo2o=";
  };

  build-system = [ setuptools llm ];
  dependencies = [ click ];

  preCheck = ''
    export HOME="$(mktemp -d)"
  '';
  nativeCheckInputs = [ pytestCheckHook ];

  pythonImportsCheck = [ "llm_commit" ];
}
