## README

RangeR package for calculating the effective range of Innovasea acoustic receivers, along with other handy functions for easy manipulation of acoustic telemetry data. Useful function include distance between receiver calculations, find closest neighbouring receiver, geodetic to utm conversions and more.

Step to calculate Dift Range Test

1.  parse logfiles from stationary receiver using FathomR::parse_fathom_files()
2.  select devices used in the test using select_devices()
3.  get the start and end time of the deployment using get_deployment_time()
4.  create a station vector listing Long,Lat
5.  select the range test Full_ID using select_range_transmitter(det)
6.  extract transmission from the VR100 log using extract_VR100_transmission
7.  extract the detection data from the stationery receiver using drift_detections()
8.  compile drift analysis using compile_drifts()
