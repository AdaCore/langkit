"""
This represents the core of the expressions parts of the DSL, that will be used
notably to define properties on AST nodes.

- Users create trees of AbstractExpression subclasses instances, and wrap them
  in Property instances in ASTNodeType subclasses.
- Code generation (Property.render) is done in two steps. First,
  AbstractExpression.construct returns ResolvedExpression trees which are bound
  to specific ASTNodeType and Field classes.
- Finally, those ResolvedExpression trees are then used to generate concrete
  code for properties in the generated library.
"""

# pyflakes off
from langkit.expressions.base import *

from langkit.expressions.analysis_units import *
from langkit.expressions.astnodes import *
from langkit.expressions.boolean import *
from langkit.expressions.collections import *
from langkit.expressions.envs import *
from langkit.expressions.logic import *
from langkit.expressions.structs import *
# pyflakes on
