.. _unparsing_configuration_file_format:

Unparsing configuration file format
===================================

The unparsing configuration is a JSON file that provides "document templates",
i.e. patterns to generate Prettier documents. Langkit's unparsing engine uses
these templates to turn a given syntax tree to a Prettier document, and then
delegates the final transformation of this document to text to Prettier itself.

Knowledge about Prettier documents (its Intermediate Representation) is
required in order to write unparsing configuration: please refer to `Prettier's
own documentation
<https://github.com/prettier/prettier/blob/main/commands.md>`_.


.. TODO:: migrate the documentation from
   ``langkit_support-generic_api-unparsing.ads``.
