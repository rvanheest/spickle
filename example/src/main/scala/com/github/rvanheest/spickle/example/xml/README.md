This package contains a number of examples on how to use the `XmlParser`, `XmlSerializer` and `XmlPickle`.

* **[person/](./person)** contains a good first example on how to use the parsers, serializers and
  pickles for reading an XML structure into an object model and writing the object back as _the same_ XML
* **[baseball/](./baseball)** shows how to deal with XML structures that contain lots of attributes.
* **[all/](./all)** demonstrates how to handle an [`<xs:all/>`](https://www.w3schools.com/xml/el_all.asp)
  element using the `fromAll` builder methods. See also the related XML files in the [resources](../../../../../../../resources/all).
* **[xsChoiceMany](./xsChoiceMany)** demonstrates how to handle an [`<xs:choice minOccurs="0" maxOccurs="unbounded"/>`](https://www.w3schools.com/xml/el_choice.asp)
  element using the `collect` operator. See also the related XML and XSD files in the [resources](../../../../../../../resources/xs-choice-many).
