# Code documentation guidelines

This document discusses guidelines for writing package and module
level documentation. For explicit standards on documentation and
general style, refer to the
`[style guide](https://github.com/tweag/guides/tree/master/style/Haskell.md)`.

We define code documentation of a package to be the comments in all
the source files in the package, plus any examples, plus any text
files explaining the package (e.g. the README).

Code documentation should be useful to both users and maintainers of a
package. Documentation must:

- Provide an overview of what problem each package and module solves.
- Explain the strategy used to solve the problem.
- Explain the details of the implementation where the intention of the
  author is not readily apparent from reading the code.
- Point to examples and material explaining how to use a package.
- Help find the code responsible for performing a specific task.
- Point to resources to learn the libraries and frameworks used by a package.

## Text files

All packages should offer a README with a synopsis of what problem the
package solves and the strategy it takes to solving it.

**Example**

From the [inline-java](https://github.com/tweag/inline-java) readme:

    The Haskell standard includes a native foreign function interface
    (FFI). Using it can be a bit involved and only C support is
    implemented in GHC. inline-java lets you call any JVM function
    directly, from Haskell, without the need to write your own foreign
    import declarations using the FFI. In the style of inline-c for C
    and inline-r for calling R, inline-java lets you name any function
    to call inline in your code. It is implemented on top of the
    [jni](https://github.com/LeapYear/leapyear/blob/master/dep/inline-java/jni)
    and [jvm](https://github.com/LeapYear/leapyear/blob/master/dep/inline-java/jvm)
    packages and makes use of
    [quasi-quotations](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell-quasi-quotation).
    ...
    inline-java can be used to build a Haskell program which starts
    the JVM and communicates to it through the Java Native Interface
    (JNI). Much of the low level details of using the JNI are hidden
    from the user.
    
    It is also possible to have the JVM load a Haskell library and
    invoke Haskell functions that can interact with the JVM using java
    quasi-quotations, e.g. what
    [sparkle](https://github.com/tweag/sparkle) does.
    
    A java quasiquotation ensures that Haskell and Java agree on the
    values that are passed between the runtimes, causes the java
    compiler to produce the Java bytecode, and then generates the
    necessary JNI calls to load and execute the bytecode in the JVM.
    
    inline-java uses whatever java compiler is found in the PATH
    environment variable.

In principle, the author is free to include more documentation in the
README or to put it in separate files. If it is placed in other files
though, these files should be referenced from the README.

The README is considered to be the root of all the hierarchy of text
files explaining a package. This hierarchy must include pointers to:

- any training materials,
    - Tests could serve as examples
    - Explanatory notes might be included in the API reference
      documentation
    - Acceptance criteria: The materials give an overview of how the
      public functions and interfaces of the package are used together
      to solve the problem.
- design notes,
    - They might live as comments in the code
    - Acceptance criteria
        - The notes give an overview of how the internal functions and
          interfaces of the package are combined to solve the problem.
        - The notes explain why the package is implemented as it is
          when there are other possible choices.
- the main entry points in the code for understanding the
  implementation,
- resources for learning the libraries and frameworks on which the
  package depends.

**Example**

    For more details on how to use inline-java, there is this
    [tutorial](https://www.tweag.io/posts/2017-09-15-inline-java-tutorial.html).
    ...
    Start navigating the code at the note "Implementation strategy" in
    [Language.Java.Inline](https://github.com/tweag/inline-java/src/Language/Java/Inline.hs).

The links to jni, jvm and quasi-quotations in the GHC user guide
provide further references to understanding Java, the JNI and
quasi-quoters.

Packages implementing servers, command line tools or other
executables should explain how to configure and execute them.

## In-code documentation

Information about the code can be conveyed in a few ways, presented
here in order of preference.

1. Types
2. Function and variable names
3. Conventions
4. Comments (please refer to [Haddock documentation](https://haskell-haddock.readthedocs.io/en/latest/markup.html#))

Whatever the chosen way, the following facts should be communicated
for each definition in the code.

Data types

- The meaning of the type
    - What do the values of the type represent in the problem domain?
    - If the type has type parameters
        - What are they meant to be instantiated to?
        - Do they correspond to some part of the problem domain as well?
- Data constructors
    - How do they relate to the problem domain?
    - What do the fields represent?
    - Is the representation isomorphic to the domain entities or are
      there some values that are to be considered invalid?
    - Are there any properties relating the values of the various fields?
- Stateful types (**example**: [handles](http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html#t:Handle))
    - What are the valid states?
    - How do transitions occur?

    **Example**

        -- | An occurrence of a java quasi-quotation
        --
        -- The polymorphic type parameter starts instantiated as a general
        -- Core Type, and later changes to @SomeSing JType@ when we analyze it
        -- (see 'unliftJTypes') ahead of generating the Java source code.
        data QQOcc a = QQOcc
          { resultType :: a
          , antiQuotationTypes :: [a]
            -- | The text between the brackets @[java| ... |]@
          , input :: Text
            -- | The name of the method to generate for executing the quasiquotation
          , methodName :: Text
            -- | e.g. @[“$x”, “$y”]@ in @[java| … $x … $y ... |]@
          , antiQuotationNames :: [Text]
            -- | The line number where the quasi-quotation appears as reported
            -- by 'Language.Haskell.TH.location'
          , lineNumber :: Integer
          }

Functions

- Which preconditions and postconditions do they have?
- Are there any properties relating them to other functions?
- Do they stand for any operation in the problem domain?

**Examples**

    -- | Returns ...
    --
    -- Precondition: The Core Type must be monomorphic and must be of kind JType.
    --
    -- If mapping over QQOccs, use ‘unliftQQOccJTypes’ instead.
    unliftJType :: JTypeNames -> Type -> CoreM (SomeSing JType)
    
    -- | Returns ...
    --
    -- If mapping over multiple arguments, use ‘unliftJTypes’ instead.
    unliftQQOccJTypes :: JTypeNames -> QQOcc Type -> CoreM (QQOcc (SomeSing JType))
    
    -- | Equivalent to @findJTypeNames >>= mapM . unliftQQOccJType@
    unliftJTypes :: [QQOcc Type] -> CoreM [QQOcc (SomeSing JType)]
    
    -- | The names of 'JType' data constructors
    data JTypeNames = JTypeNames
        { nameClass :: Name
        , nameIface :: Name
        , nameArray :: Name
        , nameGeneric :: Name
        , namePrim :: Name
        , nameVoid :: Name
        , namePrimBoolean :: Name
        , namePrimByte :: Name
        , namePrimChar :: Name
        , namePrimShort :: Name
        , namePrimInt :: Name
        , namePrimLong :: Name
        , namePrimFloat :: Name
        , namePrimDouble :: Name
        }

Modules

- What criteria should be used to decide if a function or interface
  belongs to the module?

**Example**

    -- | Definitions that depend on compiler internals
    module Language.Java.Inline.Internal.Magic where
    ...

## The guidelines in practice

When these guidelines should be applied to a code base which doesn't
follow them, they could be incorporated together with new changes.

- Newly added definitions (functions, types, classes and modules)
  should conform to the guidelines.
- Old definitions which are modified should conform to the guidelines,
  with the exception of refactorings or fixes which preserve the
  meaning of functions. 
- Definitions which are moved from one place to another unmodified
  don't need to conform.
- The text documentation of newly added packages should conform to
  the guidelines.
- The text documentation of existing packages that are modified should
  conform to the guidelines as much as the change is concerned. That
  is, the hierarchy of text files needs to be fleshed out sufficiently
  so the new state can be communicated to a person unfamiliar with the
  package. Examples:
    - The scope of the solution a package implements changes as a
      result of adding or removing definitions.
    - The way in which a package solves the problem changes, as the
      result of a total or partial redesign.

In particular, small bug-fixes don't need text documentation to be
updated unless it is somehow invalidated by the fix.
