# Quil LaTeX Generation using the AbstractFactory Design Pattern

---
## Context

LaTeX is more than a circuit building utility, in general, it can be thought of as a powerful document generation tool with an expansive ecosystem of libraries each with their own unique packages. The abstract factory pattern used in this design opens LaTeX to quil-rs as more than a circuit building utility. It also narrows down LaTeX to documents that are explicitly made available (product branding opportunity) and specific to quantum programs which can be easily expanded upon, and encouraged to, using this design pattern. Provided the type of document is known, this feature can generate it in LaTeX. 


---
## User Compatibility

If the user knows nothing about LaTeX but knows that the program can generate circuit diagrams that they can then copy and paste into a LaTeX renderer, they only need to call to_latex() using Python bindings. The default is to produce the same results with the same simplicity as pyQuil's to_latex feature, which is a Quantikz diagram of qubits and gates displayed on a circuit. 

With some knowledge of LaTeX, the user could also generate other documents made available, which this design encourages. For instance, if a user requests a graphical representation of a quil program that is made available, e.g. the probability density, a graph ConcreteFactory can be implemented that produces a pgfplots AbstractProduct which is defined stylistically in a, e.g. "quilplots", ConcreteProduct. 


---
## File Structure 

program/
|-- latex.rs
|-- latex/
    |-- generation.rs
    |--	generation/
        |-- diagram.rs
        |-- diagram/
            |-- tikz.rs
	        |-- tikz/
                |-- quantikz.rs
	
---
## Directory and File Descriptions

latex.rs
Defines the API to use the LaTeX generation feature.

latex/
Module containing all LaTeX generation code.

generation.rs
Defines a LaTeX AbstractFactory that builds document-specific ConcreteFactories.

generation/
Module containing all LaTeX document-building ConcreteFactories and their products.

diagram.rs
Defines a ConcreteFactory that builds AbstractDiagrams.

diagram/
Module containing the LaTeX AbstractDiagram and its ConcreteDiagrams.

tikz.rs
Defines an AbstractDiagram TikZ that is an expansive diagram building library used to build ConcreteDiagrams.

tikz/
Module containing all ConcreteDiagrams that can be built using the TikZ library.

quantikz.rs
Defines a ConcreteDiagram that is a Quantikz circuit from the TikZ library.

**snapshots/
Directories of snapshot unit tests.


---
## Keywords

LaTeX:    a document generation tool.
TikZ:     a LaTeX library used to generate a variety of LaTeX diagrams.
Quantikz: a TikZ package used to generate quantum circuits.
pgfplots: a LaTeX library used to visualize scientific/technical graphs.



---
## API Design

quil-rs to_latex feature should be as flexible as Python's optional parameters. With Python bindings this can be as simple as calling to_latex(). The method signature for the same quil-rs function is

`to_latex(self, request: Option<Request>) -> Result<String, LatexGenerationError>`.

A Request is a struct with two attributes: 

document: Document, // An enum variant representing the type of document being requested.
settings: Settings, // Specific settings for how the document is rendered.

Defaults
document: "diagram" // Specifically, a Quantikz diagram same as pyQuil. 
settings: None      // Default settings at the product level.

Note on Settings

- If settings are not specified then the default settings are defined in the ConcreteFactory if they can be generalized over all diagrams, or (inclusively) in the ConcreteProduct if they cannot be generalized but can only be applied on a specific type of diagram (e.q. Quantikz). Refer to the documentation for these specifications.

- Without specifying them, settings are inferred depending on the type of document being created. More concretely, settings for diagrams are different from settings for plots. The program determines what type of settings are being used by matching it to the requested document type. This allows the document factory to apply the settings to the document being produced.


---
## Program Flow

Example generating a default (Quantikz) LaTeX diagram

1. The Program makes a default request using the API defined in latex.rs.

2. The request is parsed in latex.rs which is then passed to the create_diagram method on the AbstractFactory (latex/generation.rs) to build a diagram of a Quil Program with no settings.

3. The AbstractFactory is then defined into a ConcreteFactory that builds diagrams (latex/generation/diagram.rs) with generalized settings.

4. The ConcreteFactory then makes a request to generate a default diagram from the TikZ AbstractProduct (latex/generation/diagram/tikz.rs).

5. The AbstractProduct is then defined defined into a ConcreteProduct that builds Quantikz diagrams (latex/generation/diagram/tikz/quantikz.rs). 

6. The Quantikz diagram is built at the ConcreteDiagram level and is returned to the Program 


---
## Conclusion

The use of the AbstractFactory pattern opens several avenues within the LaTeX ecosystem for the rendering of any kind of document that might be useful in visualizing Quil programs or even generating reports (considering the many more uses for LaTeX beyond diagram rendering). The vision is that at some point, Quil Programs may want to be documented in another form that LaTeX will likely be able to provide. In this case, the LatexFactory can be expanded to implement a new factory that can construct the desired Quil document e.g. Pgfplots, a LaTeX package used to generate scientific/technical graphs.  