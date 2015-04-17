# Downloads #

## Binaries and releases can be downloaded from the [project page at SourceForge](https://sourceforge.net/projects/dduce/). ##

The main repository of the sources is still based on this location.

# Introduction #

**DDuce** is a new Delphi code library. Since Delphi got new language features like operator overloading, attributes, generics, anonymous methods and extended RTTI it provides some new powerful tools to extend the developer's creativity.

This library introduces some new components like a generic `TDataSet` decendant which can be used on generic lists of objects and some new types and concepts leveraging the new language features that were introduced in the contemporary Delphis.

The sources depend on the following libraries and components:
  * [Spring4D](https://bitbucket.org/sglienke/spring4d)
  * [DSharp](https://bitbucket.org/sglienke/dsharp)
  * [Virtual treeview](https://code.google.com/p/virtual-treeview/)
  * [NativeXML](https://code.google.com/p/simdesign/)

In addition some lesser known but excellent open source Delphi components were ported and extended to support the latest versions of Delphi.

## TListDataSet`<...>` ##

A generic dataset component that can be used to expose a generic list as a `TDataSet`.

![TListDataSet](https://github.com/beNative/dduce/blob/master/Wiki/dduce_listdataset_demo.png)

## TXMLTree ##

A `TVirtualStringTree` descendant to visualize and edit XML data.

![TXMLTree](https://github.com/beNative/dduce/blob/master/Wiki/dduce_xmltree_demo.png)

## TVirtualDBGrid ##

A `TVirtualStringTree` descendant which can be linked to a `TDataSource` and can be used like a `TDBGrid`. This component is based on the sourcecode found at http://sourceforge.net/projects/virtualdbgrid/. The Initial Developer of the original code is **Peter Sulek**.

## TPropertyInspector ##

This component is based on the work of **Evgeny Balabuyev**. You may obtain a copy of the original code at http://www.torry.net/vcl/packs/lite/extlib.zip.

## TGridView ##

A native and very fast virtual VCL grid component. It is based on the work of **Roman M. Mochalov**. A copy of the original sources can be found at http://www.tersy.ru/~roman/download/.

## TDBGridView ##

A `TGridView` descendant which can be linked to a `TDataSource` and can be used like a `TDBGrid`. It is based on the work of **Roman M. Mochalov**.
