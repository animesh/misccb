<HTML>
<title>Atlanta Hotel Nijmegen</title>
<head>
<META HTTP-EQUIV="imagetoolbar" CONTENT="no">
	<script>
		function other_image(number) {
			var element_var = document.getElementById("nav" + number);
		
			var image_name = element_var.src.split("/");
			var length_array = image_name.length - 1;
		
		
			if (image_name[length_array] == "button1_0" + number +".gif") {
			
				element_var.src = "img/buttons/nl/button2_0" + number +".gif";
			}
			else {
			
				element_var.src = "img/buttons/nl/button1_0" + number +".gif";	
			}
		}
	</script>
	
	<style>
		.tekst {
			font-size: 10pt; 
			font-family: Verdana;
		}
	</style>
</head>


<body style='margin:0px;margin-top:50px;padding-bottom:20px;font-family:tahoma;' background='./img/bg_main.gif' style='background-attachment: fixed;' bgcolor=#820000>
	<map name="vlaggen_Map">
<area shape="rect" alt="English" coords="57,5,77,19" href="?lang=en">
<area shape="rect" alt="Deutsch" coords="31,5,51,19" href="?lang=de">
<area shape="rect" alt="Nederlands" coords="5,5,24,20" href="?lang=nl">
</map>
	<table border=0 cellpadding=0 cellspacing=0 align=center>
	<tr>
	<td>
		<table border=0 cellpadding=0 cellspacing=0 width=650>
		<!--
		<tr>
		<td width=6><img src='./img/spacer.gif' width=6 height=1 border=0 /></td>
		<td valign=top><img src="./img/vlaggen.jpg" width="87" height="25" border="0" alt="" usemap="#vlaggen_Map"></td>
		<td width=6><img src='./img/spacer.gif' width=6 height=1 border=0 /></td>
		</tr>
		-->
		<tr>
		<td width=6 rowspan=4><img src='./img/spacer.gif' width=6 height=1 border=0 /></td>
		<td valign=top bgcolor=#ffffff>
			<table border=0 cellpadding=0 cellspacing=0 width=100% style='border:1px solid #fff;'>
			<tr>
			<td><img src='./img/logo.gif' border=0 /></td>
			<td align=right><img src='./img/fotos-1.jpg' border=0 /></td>
			</tr>
			</table> 
		</td>
		<td width=6 rowspan=4 background='./img/shade_rechts.jpg'><img src='./img/spacer.gif' width=6 height=1 border=0 /></td>
		</tr>
		<tr>
		<td valign=top bgcolor=#000000><a href="/nw/index2.php"><img src="img/buttons/nl/button2_01.gif" onmouseover="other_image('1')" onmouseout="other_image('1')" id="nav1" border="0" /></a><a href="/nw/index2.php?page=grand-cafe"><img src="img/buttons/nl/button2_02.gif" onmouseover="other_image('2')" onmouseout="other_image('2')" id="nav2" border="0" /></a><a href="/nw/index2.php?page=actueel"><img src="img/buttons/nl/button2_03.gif" onmouseover="other_image('3')" onmouseout="other_image('3')" id="nav3" border="0" /></a><a href="/nw/index2.php?page=routebeschrijving"><img src="img/buttons/nl/button2_05.gif" onmouseover="other_image('5')" onmouseout="other_image('5')" id="nav5" border="0" /></a><a href="mailto:info@atlanta-hotel.nl"><img src="img/buttons/nl/button2_06.gif" onmouseover="other_image('6')" onmouseout="other_image('6')" id="nav6" border="0" /></a><a href="/nw/index2.php?page=reserveren"><img src="img/buttons/nl/button1_07.gif" onmouseover="other_image('7')" onmouseout="other_image('7')" id="nav7" border="0" /></a></td>
		</tr>
				<tr>
		<td valign=top height=450 bgcolor=#ffffff style='padding:12px;padding-right:15px;'>
<script language="JavaScript">
function goto_arrangement () {
var arrangement = document.reserveer.arrangement.value;
	
	if (arrangement == "1nacht") {
	window.location.href = "/nw/index2.php?page=reserveren&noarrangement=1";
	}
	else {
	window.location.href = "/nw/index2.php?page=reserveren&arrangement=" + arrangement;	
	}
}

function check_form() {
	if (reserveer.kamers.selectedIndex == -1) {
		window.alert('Veld \'Type kamer\' is niet correct ingevuld');
		return false;
	}
	if (!(parseInt(reserveer.overnachtingen.value) == reserveer.overnachtingen.value) || parseInt(reserveer.overnachtingen.value) <= 0) {
		window.alert('Veld \'Overnachtingen\' is niet correct ingevuld');
		return false;
	}
	if (reserveer.achternaam.value.length == 0) {
		window.alert('Veld \'Naam\' is niet correct ingevuld');
		return false;
	}
	if (reserveer.straat.value.length == 0) {
		window.alert('Veld \'Adres\' is niet correct ingevuld');
		return false;
	}
	if (reserveer.huisnummer.value.length == 0) {
		window.alert('Veld \'Adres\' is niet correct ingevuld');
		return false;
	}
	if (reserveer.postcode.value.length == 0) {
		window.alert('Veld \'Postcode / Plaats\' is niet correct ingevuld');
		return false;
	}
	if (reserveer.plaats.value.length == 0) {
		window.alert('Veld \'Postcode / Plaats\' is niet correct ingevuld');
		return false;
	}
	if (reserveer.telefoon.value.length == 0) {
		window.alert('Veld \'Telefoon\' is niet correct ingevuld');
		return false;
	}
	if (reserveer.email.value.length == 0) {
		window.alert('Veld \'E-mailadres\' is niet correct ingevuld');
		return false;
	}
	if (reserveer.aantal_personen.value.length == 0) {
		window.alert('Veld \'Aantal personen\' is niet correct ingevuld');
		return false;
	}
	return true;
}
</script>
<table cellpadding="0" cellspacing="0" width="100%" border=0>
	<tr>
		<td width="200" valign="top"><img src='http://www.atlanta-hotel.nl/nw/fotos/kamer.jpg' border=0 style='margin-bottom:9px;' /><br/><img src='http://www.atlanta-hotel.nl/nw/fotos/hotel-ext.jpg' border=0 style='margin-bottom:9px;' /><br/><img src='http://www.atlanta-hotel.nl/nw/fotos/hal.jpg' border=0 style='margin-bottom:9px;' /><br/>
		</td>
		
		<td class="tekst" style="padding-left:15px;" valign="top">
<fieldset style="padding: 8px; border: 1px solid #C00000;"><legend style="font-size: 13px; color: #600000;"><b>Fout</b></legend><div style="position: relative; top: -9px; left: -9px; width: 10px; height: 10px; line-height: 0px; background-image: url(img/corner_tl2.gif); background-repeat: no-repeat;"></div>Controleer de volgende gegevens:<br/><ul style="margin-bottom: 0px; margin-left: 20px;">  <li>De aankomstdatum of vertrekdatum die u heeft ingevuld is ongeldig</li></ul></fieldset><form method="post" name="reserveer" onsubmit="return check_form();">
<table border=0 cellpadding=10 cellspacing=0  bgcolor=#eeeeee width=100%>
<tr>
<td style='padding-right:30px;line-height:19px;'><font style='font-family:tahoma;font-size:20px;'>Reserveren</font><br/><br/><font style='font-family:tahoma;font-size:12px;'>Middels onderstaand formulier kunt u uw reservering bij Hotel-Grand Cafe Atlanta plaatsen. U ontvangt van ons een copie van uw reservering. Wanneer wij uw reservering hebben verwerkt zullen wij u hiervan een bevestiging toesturen.<br/><br/><font style='font-weight:bold;'>Reserveringen voor de komende 48 uur gelieve telefonisch!</font></td>
</tr>
<tr>
<td>
	<table cellpadding="4" cellspacing="0" border=0 class="tekst" width=100% style="font-family:tahoma;font-size: 12px;">
		<tr>
			<td width="105">Naam:</td>
			<td><select name="geslacht" style="font-family:tahoma;font-size:12px;width:25%;"><option selected>Dhr.</option><option >Mevr.</option></select><input type=text name=achternaam style="font-family:tahoma;font-size: 12px;width:74%;" value="Animesh Sharma">
			</td>
		</tr>
		
		
		<tr>
			<td>Adres: </td>
			<td><input type=text name=straat style="font-family:tahoma;font-size: 12px;width:85%;" value="Kongensgate"><input type=text style="width:15%;font-family:tahoma;font-size: 12px;" name=huisnummer value="33"></td>
		</tr>
		
		<tr>
			<td>Postcode / Plaats: </td>
			<td><input type=text style="width:30%;font-family:tahoma;font-size: 12px;" maxlength=7 name=postcode value="7012"><input type=text name=plaats style="width:70%;font-family:tahoma;font-size: 12px;" value="Trondheim"></td>
		</tr>
		
		<tr>
			<td>Telefoon: </td>
			<td><input type=text name=telefoon style="font-family:tahoma;font-size: 12px;" value="004746771250"></td>
		</tr>
		
		<tr>
			<td>E-mailadres: </td>
			<td><input type=text name=email style="font-family:tahoma;font-size: 12px;" value="sharma.animesh@gmail.com"></td>
		</tr>
	</table>
</td>
</tr>
<tr>
<td>
	
	<table border=0 cellpadding=0 cellspacing=0 width=100% align=center bgcolor=#cccccc style='font-family:tahoma;font-size:12px;'>
	<tr>
	<td style='padding:15px;padding-bottom:0px;'>
	<font style='font-family:tahoma;font-size:12px;font-weight:bold;margin-bottom:20px;'>Tarieven</font>
	</td>
	</tr>
	<tr>
	<td style='padding:15px;padding-bottom:0px;'>
			<table border=0 cellpadding=0 cellspacing=0 style='font-family:tahoma;font-size:12px;'>
	<tr><td style='padding-right:20px;'>1 persoonskamer</td><td>Euro&nbsp;</td><td align=right>64.50 per nacht</td></tr><tr><td style='padding-right:20px;'>2 persoonskamer enkel gebruik</td><td>Euro&nbsp;</td><td align=right>79.50 per nacht</td></tr><tr><td style='padding-right:20px;'>2 persoonskamer</td><td>Euro&nbsp;</td><td align=right>89.50 per nacht</td></tr><tr><td style='padding-right:20px;'>2 persoonskamer + 1 kind max. 12 jr</td><td>Euro&nbsp;</td><td align=right>115.00 per nacht</td></tr>			</table>
	</td>
	</tr>
	<tr>
	<td style='font-size:11px;color:#333333;padding:15px;'>Kamerprijzen zijn inclusief ontbijt en btw, exclusief toeristenbelasting (�&nbsp;1,76&nbsp;per&nbsp;persoon&nbsp;per&nbsp;nacht)</td>
	</tr>
	</table>


	<br/>
	<table cellpadding="4" width=100% cellspacing="0" class="tekst" style="font-family:tahoma;font-size: 12px;">
					<tr>
			<td width="105">Verblijf: </td>
			<td width=100%>
			<select name="arrangement" onchange="goto_arrangement();" style="font-family:tahoma;font-size: 12px;">
				<option value="1nacht">Overnachting(en)</option>
							</select>
			</td>
		</tr>
							<tr>
			<td valign=top>Type kamer:</td>
			<td><select name="kamers" style="font-family:tahoma;font-size:12px;" size=5>
				<option selected >1 persoonskamer</option><option >2 persoonskamer enkel gebruik</option><option >2 persoonskamer</option><option >2 persoonskamer + 1 kind max. 12 jr</option>				</select>
			</td>
		</tr>
							<tr>
			<td> Overnachtingen: </td>
			<td><input type=text maxlength=2 style="width: 40px; font-size: 12px;" name=overnachtingen value="1" onkeyup="check_vertrek();"> </td>
		</tr>
					
		<tr>
			<td valign=top>Aankomst: </td>
			<td>
							<input type=text maxlength=2 style="width: 30px; font-size: 12px;" name=dag value="27" onkeyup="check_vertrek();"> <input type=text style="width: 30px; font-size: 12px;" maxlength=2 name=maand value="12" onkeyup="check_vertrek();"> <input maxlength=4 style="width: 60px; font-size: 12px;" type=text name=jaar value="2013" onkeyup="check_vertrek();"> (dd/mm/yyyy)
			</td>
		</tr>
		
				<tr>
			<td> Vertrek: </td>
			<td>
			<script>
				function check_vertrek () {
					var dagen;
					
					if (document.reserveer.dag.value.length > 0 &&
						document.reserveer.maand.value.length > 0 &&
						document.reserveer.jaar.value.length > 3 &&
						document.reserveer.overnachtingen.value.length > 0) {
							
						vertrek = new Date();
						vertrek.setFullYear(document.reserveer.jaar.value, document.reserveer.maand.value - 1, document.reserveer.dag.value);
						
						dagen = parseInt(vertrek.getDate()) + parseInt(document.reserveer.overnachtingen.value);
						vertrek.setDate(dagen);
	
						document.reserveer.dag2.value = vertrek.getDate();
						document.reserveer.maand2.value = vertrek.getMonth() + 1;
						document.reserveer.jaar2.value = vertrek.getYear();
					}
				}
				
				function disabledfalse () {
					document.reserveer.dag2.disabled = false;
					document.reserveer.maand2.disabled = false;
					document.reserveer.jaar2.disabled = false;
		
					return true;
				}
			</script>
			<input type=text maxlength=2 style="width: 30px; font-size: 12px;" name=dag2 value="28" disabled="true"> <input type=text style="width: 30px; font-size: 12px;" maxlength=2 name=maand2 value="12" disabled="true"> <input maxlength=4 style="width: 60px; font-size: 12px;" type=text name=jaar2 value="2013" disabled="true"> (dd/mm/yyyy)</td>
		</tr>
		
		<tr>
						<td>Aantal personen: </td>
						<td><input type=text name=aantal_personen style="font-size: 12px; width: 40px;" maxlength=2 value="1"></td>
					</tr>		
		<tr>
			<td valign="top" style='padding-top:20px;'>Vragen/wensen:</td>
			<td  style='padding-top:20px;'><textarea name=opmerking style="font-family:tahoma;font-size: 12px;WIDTH:100%;height:100px;">Dear Manager,

I would like ot book 1 room for 27th night. I will reach around 10pm, please let me know if the reception is open.

Thanks,

Animesh</textarea></td>
		</tr>
		<tr>
		<td>&nbsp;</td><td style="padding-top: 10px;" align=left><input type=submit value='Reserveren' name=submit style="font-family:tahoma;font-size:12px;" onclick="disabledfalse ()"></td>
		</tr>
	</table>
</td>
</tr>

</table>
</form>

		</td>
	</tr>
</table>
 </td>
		</tr>
		<tr>
		<td valign=bottom bgcolor=#ffffff align=center><img src='./img/adres.gif' border=0 /></td>
		</tr>
		<tr>
		<td width=6><img src='./img/spacer.gif' width=1 height=6 border=0 /></td>
		<td width=6 background='./img/shade_onder.jpg'><img src='./img/spacer.gif' width=1 height=6 border=0 /></td>
		<td width=6><img src='./img/shade_hoekje.jpg' border=0 /></td>
		</table>
	</td>
	</tr>
	</table>
	<p align=center><img src='./img/eyesite.gif' border=0 /></p>
</body>
</HTML>