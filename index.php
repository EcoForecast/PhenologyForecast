<html>

<head>
<title>Phenology Forecast</title>
</head>

<body>

 <?php 
$dirname = "png/";
$images = glob($dirname."*.png");
foreach($images as $image) {
echo '<img src="'.$image.'" /><br />';
}

?>

</body>
</html>
