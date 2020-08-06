# tr181-validate
Parses the tr-181 xml spec and returns a HashMap of paramters and their syntaxes, so that any upstream module can use it to validate against. 

``` 

extern crate tr181-validate;

let spec = tr181-validate::parse_spec("./tr-181-2-13-0-cwmp-full.xml".to_string()).unwrap();

let attr = spec.get("Device:2.13");

if let Some(dc) = attr.get("Device.DeviceInfo.DeviceCategory") {
  println!("{:?}", dc);
}
  


```
