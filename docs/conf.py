import sys
import os
import sphinx_rtd_theme

# Support for :base-ref:, etc.
sys.path.insert(0, os.path.abspath('.'))

version = "1.0.0"

extensions = ['sphinx.ext.extlinks', 'sphinx.ext.todo']

templates_path = ['_templates']
source_suffix = '.rst'
source_encoding = 'utf-8-sig'
master_doc = 'index'

project = u'Rel8'
copyright = u'2021 CircuitHub'
release = version  

highlight_language = 'haskell'
primary_domain = 'haskell'

exclude_patterns = ['.build', "*.gen.rst"]

# on_rtd is whether we are on readthedocs.org, this line of code grabbed from docs.readthedocs.org
on_rtd = os.environ.get('READTHEDOCS', None) == 'True'

if not on_rtd:  # only import and set the theme if we're building docs locally
    import sphinx_rtd_theme
    html_theme = 'sphinx_rtd_theme'
    html_theme_path = [sphinx_rtd_theme.get_html_theme_path()]
