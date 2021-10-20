module JsTypeValidation.ValidParsingTable
let rules = Map [-27,["value";"value";"|";"value"];-26,["value";"object"];-25,["value";"array"];-24,["value";"TYPE"];-23,["value";"QUOTE"];-22,["value";"NUMBER"];-21,["value";"NULL"];-20,["value";"ID"];-19,["value";"BOOLEAN"];-18,["properties";"properties";",";"prop"];-17,["properties";"prop"];-16,["prop";"key";":";"value"];-15,["prop";"key"];-14,["object";"{";"}"];-13,["object";"{";"properties";"}"];-12,["object";"{";"properties";",";"...";"}"];-11,["object";"{";"...";"}"];-10,["key";"QUOTE"];-9,["key";"ID"];-8,["elements";"value"];-7,["elements";"elements";",";"value"];-6,["array";"[";"elements";"]"];-5,["array";"[";"elements";",";"...";"]"];-4,["array";"[";"elements";",";"...";",";"elements";"]"];-3,["array";"[";"]"];-2,["array";"[";"...";"]"];-1,["array";"[";"...";",";"elements";"]"];0,["";"value"]]
let actions = Map [0,Map ["BOOLEAN",36;"ID",37;"NULL",38;"NUMBER",39;"QUOTE",40;"TYPE",41;"[",2;"array",42;"object",43;"value",1;"{",22];1,Map ["",0;"|",45];2,Map ["...",3;"BOOLEAN",36;"ID",37;"NULL",38;"NUMBER",39;"QUOTE",40;"TYPE",41;"[",2;"]",8;"array",42;"elements",9;"object",43;"value",19;"{",22];3,Map [",",4;"]",7];4,Map ["BOOLEAN",36;"ID",37;"NULL",38;"NUMBER",39;"QUOTE",40;"TYPE",41;"[",2;"array",42;"elements",5;"object",43;"value",19;"{",22];5,Map [",",17;"]",6];6,Map ["",-1;",",-1;"]",-1;"|",-1;"}",-1];7,Map ["",-2;",",-2;"]",-2;"|",-2;"}",-2];8,Map ["",-3;",",-3;"]",-3;"|",-3;"}",-3];9,Map [",",10;"]",16];10,Map ["...",11;"BOOLEAN",36;"ID",37;"NULL",38;"NUMBER",39;"QUOTE",40;"TYPE",41;"[",2;"array",42;"object",43;"value",18;"{",22];11,Map [",",12;"]",15];12,Map ["BOOLEAN",36;"ID",37;"NULL",38;"NUMBER",39;"QUOTE",40;"TYPE",41;"[",2;"array",42;"elements",13;"object",43;"value",19;"{",22];13,Map [",",17;"]",14];14,Map ["",-4;",",-4;"]",-4;"|",-4;"}",-4];15,Map ["",-5;",",-5;"]",-5;"|",-5;"}",-5];16,Map ["",-6;",",-6;"]",-6;"|",-6;"}",-6];17,Map ["BOOLEAN",36;"ID",37;"NULL",38;"NUMBER",39;"QUOTE",40;"TYPE",41;"[",2;"array",42;"object",43;"value",18;"{",22];18,Map [",",-7;"]",-7;"|",45];19,Map [",",-8;"]",-8;"|",45];20,Map [",",-9;":",-9;"}",-9];21,Map [",",-10;":",-10;"}",-10];22,Map ["...",23;"ID",20;"QUOTE",21;"key",31;"prop",34;"properties",25;"}",30];23,Map ["}",24];24,Map ["",-11;",",-11;"]",-11;"|",-11;"}",-11];25,Map [",",26;"}",29];26,Map ["...",27;"ID",20;"QUOTE",21;"key",31;"prop",35];27,Map ["}",28];28,Map ["",-12;",",-12;"]",-12;"|",-12;"}",-12];29,Map ["",-13;",",-13;"]",-13;"|",-13;"}",-13];30,Map ["",-14;",",-14;"]",-14;"|",-14;"}",-14];31,Map [",",-15;":",32;"}",-15];32,Map ["BOOLEAN",36;"ID",37;"NULL",38;"NUMBER",39;"QUOTE",40;"TYPE",41;"[",2;"array",42;"object",43;"value",33;"{",22];33,Map [",",-16;"|",45;"}",-16];34,Map [",",-17;"}",-17];35,Map [",",-18;"}",-18];36,Map ["",-19;",",-19;"]",-19;"|",-19;"}",-19];37,Map ["",-20;",",-20;"]",-20;"|",-20;"}",-20];38,Map ["",-21;",",-21;"]",-21;"|",-21;"}",-21];39,Map ["",-22;",",-22;"]",-22;"|",-22;"}",-22];40,Map ["",-23;",",-23;"]",-23;"|",-23;"}",-23];41,Map ["",-24;",",-24;"]",-24;"|",-24;"}",-24];42,Map ["",-25;",",-25;"]",-25;"|",-25;"}",-25];43,Map ["",-26;",",-26;"]",-26;"|",-26;"}",-26];44,Map ["",-27;",",-27;"]",-27;"|",-27;"}",-27];45,Map ["BOOLEAN",36;"ID",37;"NULL",38;"NUMBER",39;"QUOTE",40;"TYPE",41;"[",2;"array",42;"object",43;"value",44;"{",22]]
let kernelSymbols = Map [1,"value";2,"[";3,"...";4,",";5,"elements";6,"]";7,"]";8,"]";9,"elements";10,",";11,"...";12,",";13,"elements";14,"]";15,"]";16,"]";17,",";18,"value";19,"value";20,"ID";21,"QUOTE";22,"{";23,"...";24,"}";25,"properties";26,",";27,"...";28,"}";29,"}";30,"}";31,"key";32,":";33,"value";34,"prop";35,"prop";36,"BOOLEAN";37,"ID";38,"NULL";39,"NUMBER";40,"QUOTE";41,"TYPE";42,"array";43,"object";44,"value";45,"|"]
open FSharpCompiler.Parsing
let pconfig = ParserConfig(rules, actions, kernelSymbols)