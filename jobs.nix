let rel8 = (import ./.).hsPkgs.rel8;
in { 
  rel8 = rel8.components.library; 
  tests = rel8.checks.tests; 
  doctests = rel8.checks.doctests; 
}
