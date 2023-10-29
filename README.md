![WMI query simple class for Delphi](Resources/GitHubHeader.png)
# WMI query simple class for Delphi

A Delphi class to simplify access to the WMI information.
This library is an old class I wrote back in the days when managing the WMI was a hard task, I wanted to have an easy way to query the WMI, so I decided to create a class that mimics the TDataset way of exposing records and fields.

## TWMIObject

TWMIObject is the main class which will give you all the functionalities you will need.

## Setting up a TWMIObject

Once a TWMIObject instance is created it's ready to run on your local computer, but you might want to set up some properties before querying the WMI.

```delphi
.Computer := '.';
```

This will allow you to access network computers' WMI by settings their's hostname or IP address, by default `'.'` is set, which means the object is working on local computer.

```delphi
.Namespace := 'root\CIMV2';
```

This will allow you to specify the namespace you're working in, by default `'root\CIMV2'` is set.

```delphi
.Username := '';
```

This property allows to set the username to use while querying WMI, by default it's set to an empty string, which means user executing current process will be used.

```delphi
.Password := '';
```

This property allow you to provide the password of the user to be used for querying the WMI.

## Querying a TWMIObject

There're two main approaches to retrieve data, the first is by executing a query, the second one is by requesting all instances of a class, for this reason the TWMIObject has two disting methods.

```delphi
function ExecQuery(const ASQL: String): TWMIResult;
```

This function will execute a query and results returned by it's execution will be stored in the TWMIObject.

```delphi
function InstancesOf(const AClass: String): TWMIResult;
```

This function will retrieve all instances of a given class, which will be stored in the TWMIObject.

Both those functions will return a TWMIResult value which is reporting if the operation was successful or not and in this last case, which error occurred.
Possible values of TWMIResult are described in the table below.

| Value | Description |
|----|----|
|`wmirOk`|Operation was successful, all data is ready in the object instance to be read.|
|`wmirNoConnection`|No connection with the specified `Computer` has been established.|
|`wmirException`|An exception has been thrown while trying to execute a query.|
|`wmirEmptyResult`|The query produced an empty recordset.|
|`wmirEmptyObject`|The records returned by the query have no fields.|
|`wmirAccessDenied`|Access is denied for the user querying WMI.|
|`wmirFailed`|Query failed.|
|`wmirInvalidParameter`|Query had invalid parameters.|
|`wmirInvalidQuery`|Query is invalid.|
|`wmirInvalidQueryType`|Query type is invalid.|
|`wmirInvalidClass`|Specified class is invalid.|
|`wmirOutOfMemory`|Not enough memory to complete operation.|

## Status of the object

To know the status of the object you can check the `.Mode` property, which can be one of these values:

|Value|Description|
|----|----|
|`wmimNone`|The object is closed, no query nor class instance has been searched.|
|`wmimQuery`|The object contains data coming from a query.|
|`wmimClass`|The object contains data representing instances of a class.|

## Further error details

Each time an exception occurs, the exception will be recorded and you can access it through the `.Error` property.

This property is `nil` when no error occurred.

## Browsing the records

A similar way to the TDataset class is provided for browsing through records.

| Method | Description |
|----|----|
|`.First`|Moves to the first record in the recordset. Returns `true` if successfully moved, `false` if the recordset is empty.|
|`.Prev`|Moves to the previous record in the recordset. Returns `true` if successfully moved, `false` if it's already on recordset starting record or the recordset is empty.|
|`.Next`|Moves to the next record in the recordset. Returns `true` if successfully moved, `false` if it's already at recordset end or the recordset is empty.|
|`.Last`|Moves to the last record in the recordset. Returns `true` if successfully moved, `false` if it's already at recordset end or the recordset is empty.|

Also, you have properties to know where you are and how the recordset is.

| Property | Type | Description |
|----|----|----|
|`.BoF`|`boolean`|Returns if recordset is on the first record.|
|`.Count`|`integer`|Returns the number of records currently present in the recordset.|
|`.EoF`|`boolean`|Returns if recordset is on the first record.|
|`.IsEmpty`|`boolean`|Returns if recordset is on the first record.|

## Browsing the fields

The object exposes the `.Fields` property which is a TCollection property with which you can access the fields in the recordset.

It also exposes two utility functions.

```delphi
function FieldExists(const AName: String): Boolean;
```

This function will return `true` if a field with given name exists, `false` otherwise.

```delphi
function FieldByName(const AName: String): TWMIField;
```

This function will return the field itself given the field name.

## Accessing the values

Once you have a `TWMIField` object you can access the data.

First of all the most important property of the field is `.FieldType` which represents the kind of data contained, this can be one of the following values.

|Value|Description/Delphi type|
|----|----|
|`ftUnknown`|The type of this field was not identified.|
|`ftBoolean`|`boolean`|
|`ftDateTime`|`TDateTime`|
|`ftExtended`|`Extended`|
|`ftInteger`|`integer`|
|`ftString`|`string`|

Two other properties are important when dealing with values.

The `.IsNull` property returns a boolean value indicating if the value in the field equals `nil`.

The `.IsArray` property returns a boolean value indicating if the field contains an array of values.

For each of these types an utility function is provided.

```delphi
function AsBoolean: Boolean;
```

Reads the value as a `boolean`.

```delphi
function AsDateTime(const ADefault: TDateTime = 0.0): TDateTime;
```

This function returns a `TDateTime` parsing the field as a string, if parsing fails, the default given parameter is returned.

This function also has an overloaded which allows to know at what point parsing failed.

```delphi
function AsDateTime(var AOffset: Integer; const ADefault: TDateTime = 0.0): TDateTime;
```

The `AOffset` variable will be set at the position where an invalid character was found.

```delphi
function AsExtended(const ADefault: Extended = 0.0): Extended;
```

Reads and parses the field value as a floating point value, if the parsing fails, the given `ADefault` value is returned.

```delphi
function AsInteger(const ADefault: Int64 = -1): Int64;
```

Reads and parses the field value as an integer value, if the parsing fails, the given `ADefault` value is returned.

```delphi
function AsString: String; 
```

Read the field value as a string, this could also be garblish binary data.

```delphi
function AsRaw: String;
```

The `.AsRaw` function could appear to be doing the same thing `.AsString` does, but when handling arrays this returns the raw value.

## Dealing with array of values

When the `.IsArray` property is `true`, you can check the number of items in the array by checking the `.Count` property, which returns an integer value.

Also, two of the above mentioned functions have an overloaded version of the function to access the value at the n-th index.

```delphi
function AsInteger(const AIndex: Integer; const ADefault: Int64): Int64;
```

```delphi
function AsString(const AIndex: Integer): String;
```

## Recreating WbemScripting_TLB.pas

The TWMIObject class relies on the imported type library for WMI.

Since usually Delphi wizard to import type libraries gets better with each version of the product, you should probably replace the one I'm providing here, imported using Delphi 11.1, using your version.

From the Delphi IDE, select "Component", then "Import Component...", select the "Import a Type Library" option and click on "Next >>", from the upcoming list choose "Microsoft WMI Scripting V1.2 Library" and
click on "Next >>" again.

Add a checkmark on the "Generate Components Wrappers" option, this will automatically switch the "Palette Page" field to "ActiveX", don't worry, you don't need to register them.
Choose output folder in "Unit Dir Name" field browsing to the destination folder using the "..." button, then click "Next >>".
Select "Create Unit", and click "Finish", after a short delay the unit will be generated.

You can delete the WbemScripting_TLB.dcr file since we're not registering these components.

## Version history
| Version | Release date | Description |
|---------|--------------|-------------|
| 1.0 | 200? | First version created, never released to public. |
| 1.0 | 2023-10-29 | Published on GitHub. |