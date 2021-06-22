The LExSIS structure is a specific yaml format, for use in interview generation within L4-Docassemble. The LExSIS file primarily contains information regarding the objects to be used; specifying the associated docassemble datatype, arity of the object, the associated sCASP encoding, and other natural language options.

It is to be used in conjunction with an associated s(CASP) file, so that the interview generator has information regarding the relationships of objects (& any child attributes), as well as how to reason about them. 

### LExSIS Specification
A LExSIS file consists of 4 main key-value pairs. The keys are `rules`, `query`, `data` and `terms`.

A simple LExSIS file would look like this:
```yaml
rules: rps.pl
query: win(game,player1)
data:
- name: game
  type: String
  minimum: 0 
  encodings:
    - game(X)
  attributes:
    - name: participate_in
      type: Object
      encodings:
        - participate_in(X,Y)
      source: player
			
    - name: win
      type: Object
      encodings:
        - win(X,Y)
      source: player

  - name: player
    type: String
    minimum: 0 
    encodings:
      - player(X)
    attributes:
      - name: throw
        type: Object
        minimum: 0
        encodings:
          - throw(X,Y)
        source: sign

  - name: sign
    type: Enum
		options: 
			rock: Rock
			paper: Paper
			scissors: Scissors
    encodings:
      - sign(X)
    attributes:
      - name: beat
        type: Boolean
        minimum: 0
				encodings:
          - beat(X,Y)
        source: sign
				
				
terms: 
	

```


#### Rules
The `rules` header should contain the filename of the associated s(CASP) file. The s(CASP) file contains the rule you want. The file should be located in the same folder of the package that the yaml file is.

#### Query
The `query` header should contain a valid s(CASP) query (without the trailing period)

#### Data
The `data` header is where data about the docassemble interview should be specified. This includes what data should be collected, in what order, and what the corresponding s(CASP) encoding is. 

It should contain a list of data elements, each containing a `name` key, a `type` key, and an `encodings` header. Depending on the type of the element, it can also contain additional necessary headers. Each data element can also optionally include an `attributes` header (for child data elements), a `minimum`/`maximum`/`exactly` element (for arity), or a `ask`/`tell`/`any`/`another` header for managing the vocabulary used in the questions.

##### Name

If you use underscores in the names, docassemble will replace those with spaces when displaying them to the user.

##### Types

The types available are:

-   Boolean
-   Continue
-   String
-   Enum
-   Number
-   Date
-   DateTime
-   Time
-   YesNoMaybe
-   File
-   Object

Some of the types make additional dictionary entries mandatory for that data element.

###### Enum

A data element of type Enum requires a `choices:` key, the value of which must me a Python dictionary where the keys are the values that should be set, and the values are what should be displayed to the user.

###### Object

A data element of type Object requires a `source:` key, the value of which must be the `name` of a root data element (a data element that is not an attribute of another data element) in the same file.

###### Encodings

The encodings element is a list of s(CASP) statements that should be added to the rules with regard to the object that is collected, or the object of which it is an attribute, before the query is sent to the s(CASP) reasoner.

Again, the s(CASP) statements do not have trailing periods.

If you use the letter `X` in your encoding, it will be replaced with the value of the data element it is contained in. If you use the letter `Y` in your encoding, it will be replaced with the value of the data element of which this data element is an attribute.

For example, if you use this code:

```
data:
  - name: thing
    type: String
    minimum: 0
    encodings:
      - thing(X)
    attributes:
      - name: Human
        type: Boolean
        encodings:
          - human(Y)
```

If you create a list of two "things," "bob" and "superman", and indicate that only bob is human, then prior to sending the query to s(CASP) the following code will be added to the rules:

```
thing(bob).
human(bob).
thing(superman).
```

Note that because the encoding for "human" refers to Y, it is the value of the parent object "bob", and not the value of the boolean "True" that gets inserted into the s(CASP) statement.

###### Attributes

The `attributes:` part of a data element is a list of data elements that should be collected "inside" this data element. Note that docassemble will complain about attributes nested more than 5 levels deep.

###### Arity: Minimum, Maximum, Exactly

In order to tell docassemble how many of each data elements is relevant to your application, you can specify the cardinality with `minimum`, `maximum`, and `exactly`.

If none of these exist, docassemble assumes that your data element should be collected exactly once, as if you had specified `exactly: 1`.

If you specify a minimum and no maximum, or a minimum and a higher maximum, the data element will be collected as a list.

You can create an optional single attribute by using `minimum: 0` and `maximum: 1`.

##### User Interface: Ask, Tell, Any, and Another

By default, docassemble-l4 will generate questions to display to the user on the basis of the name you give to your data element. If you would like to customize the questions that are displayed to the user you can do that by setting the `ask`, `any` and `another` attributes on your data element.

The `ask` element sets out the question that should be displayed when collecting the value of the data element. If you use the letter `Y` in this question, in the interview it will be replaced with the value of the parent data element.

The `tell` element sets out the text that should be used to describe the current element when it is referred to by its attributes. In this text the letter `Y` will be replaced with the tell value of the parent if there is one. In this text the letter `X` will be replaced with the value of this element. If you do not specify a `tell` value, the value of the object is used by itself.

The `any` and `another` elements are used only if the data element is a list. If the list does not have a minimum or the minimum is zero, the user will be asked whether or not the list has any elements in it. The `any` element specifies how that question should be asked. Likewise, the `another` element speifies the question that should be asked when asking if the list has any additional elements.

For example, if you are collecting a list of sports that a person plays, you might write this:

```
data:
  - name: Person
    type: String
    tell: the person named X
    attributes:
      - name: sport
        type: String
        minimum: 0
        any: Does Y play any sports?
        ask: What sport does Y play?
        another: Does Y play any other sports?
```

If the user enters "Janet" as the person's name, the user will be asked "Does the person named Janet play any sports?", "What sport does the person named Janet play?", and "Does the person named Janet play any other sports?" as required.

#### Terms

```
terms:
  - mortal: |
      capable of dying
```

The terms element is a list of key-value pairs where the key is the text that is used as a linked defined term, and the value is the text that should be displayed as the definition. The definition can include markdown formatting. In order to use this in the explanations, the same terms will need to be surrounded by curly braces wherever they are generated in your interview or s(CASP) code (e.g. `#pred mortal(X) :: "@(X) is {mortal}".`)

#### Options

```
options:
  show models: True
  answers: all
```

If you would like to specify options available in docassemble-scasp, you can do that using the `options` element. There are two configurations available, the number of answers to display (`answers`), and whether or not to display answer set models for the answers (`show models`). If `answers: all` or `answers: 0` is specified, all answers will be displayed, but this is also the default. If a number other than `0` is provided, only that number of answers will be displayed if they are available.

If `show models: True` is specified as an option, models will be displayed with the answers. `False` is the default.
