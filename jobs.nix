let project = import ./.;
in { 
  rel8 = project.hsPkgs.components.library; 
  tests = project.hsPkgs.components.checks.tests; 
}
