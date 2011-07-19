
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">
  <style type='text/css'>
<!--
.transMenu {
 position:absolute ; 
 overflow:hidden; 
 left:-1000px; 
 top:-1000px; 
}
.transMenu .content {
 position:absolute  ; 
}
.transMenu .items {
 border: 0px none #ececec ; 
 position:relative ; 
 left:0px; top:0px; 
 z-index:2; 
}
.transMenu  td
{
 padding: 2px 5px 2px 5px !important;  
 font-size: 10px !important ; 
 font-family: Verdana, Arial, Helvetica, sans-serif !important ; 
 text-align: left !important ; 
 font-weight: normal !important ; 
 color: #860000 !important ; 
} 
#subwrap 
{ 
 text-align: left ; 
}
.transMenu  .item.hover td
{ 
 color: #860000 !important ; 
}
.transMenu .item { 
 text-decoration: none ; 
 cursor:pointer; 
 cursor:hand; 
}
.transMenu .background {
background-image:none; background-color: #ececec !important ; 
 position:absolute ; 
 left:0px; top:0px; 
 z-index:1; 
 opacity:0.95; 
 filter:alpha(opacity=95) 
}
.transMenu .shadowRight { 
 position:absolute ; 
 z-index:3; 
 top:3px; width:2px; 
 opacity:0.95; 
 filter:alpha(opacity=95)
}
.transMenu .shadowBottom { 
 position:absolute ; 
 z-index:1; 
 left:3px; height:2px; 
 opacity:0.95; 
 filter:alpha(opacity=95)
}
.transMenu .item.hover {
background-image:none; background-color: #ffffff !important ; 
}
.transMenu .item img { 
 margin-left:10px !important ; 
}
table.menu {
 top: 0px; 
 left: 0px; 
 position:relative ; 
 margin:0px !important ; 
 border: 0px none #ececec ; 
 z-index: 1; 
}
table.menu a{
 margin:0px !important ; 
 padding: 0px 5px 0px 5px !important ; 
 display:block !important; 
 position:relative !important ; 
}
div.menu a,
div.menu a:visited,
div.menu a:link {
 font-size: 11px !important ; 
 font-family: Verdana, Arial, Helvetica, sans-serif !important ; 
 text-align: left !important ; 
 font-weight: normal !important ; 
 color: #000000 !important ; 
 text-decoration: none !important ; 
 margin-bottom:0px !important ; 
 display:block !important; 
 white-space:nowrap ; 
}
div.menu td {
 border-bottom: 1px none #ececec ; 
 border-top: 1px none #ececec ; 
 border-left: 1px none #ececec ; 
background-image:none; background-color: #ececec !important ; 
} 
div.menu td.last {
 border-right: 1px none #ececec ; 
} 
#trans-active a{
 color: #000000 !important ; 
background-image:none; background-color: #ececec !important ; 
} 
#menu a.hover   { 
background-image:none; color: #000000 !important ; 
 background-color: #ececec !important ; 
}
#menu span {
 display:none; 
}

-->
</style>

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> This package has been developped by the CGIAR <a href="http://vslp.org" title="Systemwide Livestock Programme">Systemwide Livestock Programme</a>. </p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
