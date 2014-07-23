<html>

<head>
<title>Phenology Forecast</title>
</head>

<body>

<?php
$row = 0;
$dirname = "png/";
if (($handle = fopen("site_metadata.csv", "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
      if($row > 0){
        echo "<h1> site $row: $data[0] $data[2] $data[3] <br /></h1>\n";
	$images = glob($dirname."ParticleFilterForecast.".$row."*.png");
	$timage = glob($dirname."ThresholdForecast.".$row."*.png");
//	foreach($images as $image) {
//	   echo '<img src="'.$image.'" /><br />';
//	}
	echo '<img src="'.end($images).'" /><img src="'.end($timage).'" /><br />';
      }
      $row++;
    }
    fclose($handle);
}
?>

</body>
</html>
