# Part 2 of the test

Goal: create a Pharo program that generates a simple documentation equivalent to JavaDoc for classes in a package. The documentation includes details about a class's superclass, subclasses, instance variables, and methods.

---

## Task Requirements
1. **Generate Documentation for a Class**: Print details of a given class, including:
    - Superclass
    - Subclasses
    - Instance variables
    - Methods
2. **Test the Code**: Create a test class to verify the functionality of the documentation methods.

---

## Implementation Outline

### **1. InfoFile Class**
This class contains methods for retrieving and printing class information.

```smalltalk
Object subclass: #InfoFile
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'PackageDocumenter'.


InfoFile class>>superclassOf: aClass
    ^ aClass superclass.

InfoFile class>>subclassesOf: aClass
    ^ aClass subclasses.

InfoFile class>>instanceVariablesOf: aClass
    ^ aClass instVarNames.

InfoFile class>>methodsOf: aClass
    ^ aClass selectors.

InfoFile class>>documentClass: aClass
    Transcript show: 'Class: ', aClass name; cr.
    Transcript show: 'Superclass: ', (self superclassOf: aClass) name; cr.
    Transcript show: 'Subclasses: ', ((self subclassesOf: aClass) collect: #name) asString; cr.
    Transcript show: 'Instance Variables: ', (self instanceVariablesOf: aClass) asString; cr.
    Transcript show: 'Methods: ', (self methodsOf: aClass) asString; cr.
```

### **2. InfoFileTest Class**
This test class validates the functionality of the `InfoFile` methods.

```smalltalk
TestCase subclass: #InfoFileTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'PackageDocumenterTests'.

testInstanceVariablesOf
    | info instanceVars classToTest |
    info := InfoFile new.
    classToTest := Point.
    instanceVars := info instanceVariablesOf: classToTest.
   "Assert that 'x' is an instance variable of Point"
    self assert: (instanceVars includes: 'x').
    "Assert that 'y' is an instance variable of Point"
    self assert: (instanceVars includes: 'y').

testMethodsOf
    | info methods classToTest |
    info := InfoFile new.
    classToTest := String.
    methods := info methodsOf: classToTest.
    "Assert that 'asUppercase' is a method of String"
    self assert: (methods includes: #asUppercase).
    "Assert that 'asLowercase' is a method of String"
    self assert: (methods includes: #asLowercase).

testSubclassesOf
    | info subclasses classToTest |
    info := InfoFile new.
    classToTest := Object.
    subclasses := info subclassesOf: classToTest.
    "Assert that InfoFile is a subclass of Object"
    self assert: (subclasses includes: InfoFile ).

testSuperclassOf
    | info superClass classToTest |
    info := InfoFile new.
    classToTest := InfoFile.
    superClass := info superclassOf: classToTest.
    "Assert that the superclass of InfoFile is Object"
    self assert: superClass = Object.


```

---

## Screenshot of Test Results
Below is an example screenshot of successful test execution:

![Test Results](./PharoDocumenter.png)

---

## Pharo Image File
Include the Pharo image file used for this project to ensure reproducibility:

- **Filename**: `PharoDocumenter.image`

---

### Notes
- The `Transcript` is used to print documentation details. Make sure the Transcript window is open during execution.
