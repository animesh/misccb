#  Name:
#     read_sfunwiz.pl-- reads the #defines values from the S-function generated by the wizard
#  Usage:
#    to be called by $MATLAB/toolbox/simulink/simulink/sfunctionwizard.m
#
# Copyright 1990-2004 The MathWorks, Inc.
# $Revision: 1.8.4.4 $

($SfunName, $sfbRev)= @ARGV;

if ($sfbRev == '1.0') {
# V1.0
  @DefinesWizardData =("#define INPUT_0_WIDTH",
		       "#define INPUT_0_FEEDTHROUGH",
		       "#define OUTPUT_0_WIDTH",
		       "#define NPARAMS",
		       "#define SAMPLE_TIME_0",
		       "#define NUM_DISC_STATES",
		       "#define DISC_STATES_IC",
		       "#define NUM_CONT_STATES",
		       "#define CONT_STATES_IC",
		       "#define SFUNWIZ_GENERATE_TLC",
		       "#define SOURCEFILES",
		       "#define PANELINDEX");
  
  @MATLABVariable = ( "ad.SfunWizardData.InputPortWidth",
		      "ad.SfunWizardData.DirectFeedThrough",
		      "ad.SfunWizardData.OutputPortWidth",
		      "ad.SfunWizardData.NumberOfParameters",
		      "ad.SfunWizardData.SampleTime",
		      "ad.SfunWizardData.NumberOfDiscreteStates",
		      "ad.SfunWizardData.DiscreteStatesIC",
		      "ad.SfunWizardData.NumberOfContinuousStates",
		      "ad.SfunWizardData.ContinuousStatesIC",
		      "ad.SfunWizardData.GenerateTLC",
		      "ad.SfunWizardData.LibraryFilesText",
		      "ad.SfunWizardData.PanelIndex");
} else {

  @MATLABVariable = ( "ad.SfunWizardData.InputPortWidth",
		      "ad.SfunWizardData.DirectFeedThrough",
		      "ad.SfunWizardData.OutputPortWidth",
		      "ad.SfunWizardData.NumberOfParameters",
		      "ad.SfunWizardData.SampleTime",
		      "ad.SfunWizardData.NumberOfDiscreteStates",
		      "ad.SfunWizardData.DiscreteStatesIC",
		      "ad.SfunWizardData.NumberOfContinuousStates",
		      "ad.SfunWizardData.ContinuousStatesIC",
		      "ad.SfunWizardData.InputDataType0",
		      "ad.SfunWizardData.OutputDataType0",
		      "ad.SfunWizardData.InputSignalType0",
		      "ad.SfunWizardData.OutputSignalType0",
		      "ad.SfunWizardData.InFrameBased0",
		      "ad.SfunWizardData.OutFrameBased0",
		      "ad.SfunWizardData.TemplateType",
		      "ad.SfunWizardData.Input0DimsCol",
		      "ad.SfunWizardData.Output0DimsCol",		     
		      "ad.SfunWizardData.GenerateTLC",
		      "ad.SfunWizardData.LibraryFilesText",
		      "ad.SfunWizardData.PanelIndex"
		     );
  @inPortsStruct = ("ad.SfunWizardData.InputPorts.Name",
		    "ad.SfunWizardData.InputPorts.Row",
		    "ad.SfunWizardData.InputPorts.Col",
		    "ad.SfunWizardData.InputPorts.DataType",
		    "ad.SfunWizardData.InputPorts.Complexity",
		    "ad.SfunWizardData.InputPorts.Frame",               
                    "ad.SfunWizardData.InputPorts.Bus",
                    "ad.SfunWizardData.InputPorts.Busname",            
                    "ad.SfunWizardData.InputPorts.Dims",
                    "ad.SfunWizardData.InputPorts.IsSigned",
                    "ad.SfunWizardData.InputPorts.WordLength",
                    "ad.SfunWizardData.InputPorts.FractionLength",
                    "ad.SfunWizardData.InputPorts.FixPointScalingType",
                    "ad.SfunWizardData.InputPorts.Slope",
                    "ad.SfunWizardData.InputPorts.Bias"
  );


  @inDefinesBuilderDataBegin =("#define IN_PORT_",
			       "#define INPUT_",
			       "#define INPUT_DIMS_",
			       "#define INPUT_",
			       "#define INPUT_",
			       "#define IN_",
                               "#define IN_",
                               "#define IN_",
                               "#define IN_",
                               "#define IN_",
                               "#define IN_",
                               "#define IN_",
                               "#define IN_",
                               "#define IN_",
                               "#define IN_",
			       "#define IN_");

  @inDefinesBuilderDataEnd =("_NAME",
			     "_WIDTH",
			     "_COL",
			     "_DTYPE",
			     "_COMPLEX",
			     "_FRAME_BASED",
                             "_BUS_BASED",
                             "_BUS_NAME",
			     "_DIMS",
                             "_ISSIGNED",
                             "_WORDLENGTH",
                             "_FRACTIONLENGTH",
                             "_FIXPOINTSCALING",
                             "_SLOPE",
                             "_BIAS");

  @outPortsStruct = ("ad.SfunWizardData.OutputPorts.Name",
		     "ad.SfunWizardData.OutputPorts.Row",
		     "ad.SfunWizardData.OutputPorts.Col",
		     "ad.SfunWizardData.OutputPorts.DataType",
		     "ad.SfunWizardData.OutputPorts.Complexity",
		     "ad.SfunWizardData.OutputPorts.Frame",
                     "ad.SfunWizardData.OutputPorts.Bus",
                     "ad.SfunWizardData.OutputPorts.Busname",
		     "ad.SfunWizardData.OutputPorts.Dims",
                    "ad.SfunWizardData.OutputPorts.IsSigned",
                    "ad.SfunWizardData.OutputPorts.WordLength",
                    "ad.SfunWizardData.OutputPorts.FractionLength",
                    "ad.SfunWizardData.OutputPorts.FixPointScalingType",
                    "ad.SfunWizardData.OutputPorts.Slope",
                    "ad.SfunWizardData.OutputPorts.Bias");
  
  @outDefinesBuilderDataBegin =("#define OUT_PORT_",
				"#define OUTPUT_",
				"#define OUTPUT_DIMS_",
				"#define OUTPUT_",
				"#define OUTPUT_",
				"#define OUT_",
                                "#define OUT_",
                                "#define OUT_",
                                "#define OUT_",
                                "#define OUT_",
                                "#define OUT_",
                                "#define OUT_",
                                "#define OUT_",
                                "#define OUT_",
				"#define OUT_");
  
  @outDefinesBuilderDataEnd =("_NAME",
			      "_WIDTH",
			      "_COL",
			      "_DTYPE",
			      "_COMPLEX",
			      "_FRAME_BASED",
                              "_BUS_BASED",
                              "_BUS_NAME",
			      "_DIMS",
                              "_ISSIGNED",
                              "_WORDLENGTH",
                              "_FRACTIONLENGTH",
                              "_FIXPOINTSCALING",
                              "_SLOPE",
                              "_BIAS");
  
  @parametersStruct = ("ad.SfunWizardData.Parameters.Name",
		     "ad.SfunWizardData.Parameters.DataType",
		     "ad.SfunWizardData.Parameters.Complexity");
  
  @ParameterDefinesBuilderDataBegin =("#define PARAMETER_",
				"#define PARAMETER_",
				"#define PARAMETER_");
  
  @ParameterDefinesBuilderDataEnd =("_NAME",
				    "_DTYPE",
				    "_COMPLEX");


  @DefinesWizardData =("#define NPARAMS",
		       "#define SAMPLE_TIME_0",
		       "#define NUM_DISC_STATES",
		       "#define DISC_STATES_IC",
		       "#define NUM_CONT_STATES",
		       "#define CONT_STATES_IC",
		       "#define SFUNWIZ_GENERATE_TLC",
		       "#define SOURCEFILES",
		       "#define PANELINDEX",
		       "#define USE_SIMSTRUCT",
		       "#define SHOW_COMPILE_STEPS",
		       "#define CREATE_DEBUG_MEXFILE",
		       "#define SAVE_CODE_ONLY");

  @MATLABVariable = ( "ad.SfunWizardData.NumberOfParameters",
		      "ad.SfunWizardData.SampleTime",
		      "ad.SfunWizardData.NumberOfDiscreteStates",
		      "ad.SfunWizardData.DiscreteStatesIC",
		      "ad.SfunWizardData.NumberOfContinuousStates",
		      "ad.SfunWizardData.ContinuousStatesIC",
		      "ad.SfunWizardData.GenerateTLC",
		      "ad.SfunWizardData.LibraryFilesText",
		      "ad.SfunWizardData.PanelIndex",
		      "ad.SfunWizardData.UseSimStruct",
		      "ad.SfunWizardData.ShowCompileSteps",
		      "ad.SfunWizardData.CreateDebugMex",
		      "ad.SfunWizardData.SaveCodeOnly");

}
$beginStr = "SFUNWIZ_defines_Changes_BEGIN";
$endStr = "SFUNWIZ_defines_Changes_END";

open(HFile, "<$SfunName") || die "Unable to open  $SfunName";
$i = 0;
while (<HFile>) {  
 my($line) = $_;

 if(/$beginStr/.../$endStr/){

   if(/\#define NUM_INPUT/) {
     @ni = split; 
     $numInputPorts =  $ni[2];
   }

   if(/\#define NUM_OUTPUTS/) {
     @sp = split; 
     $numOutputPorts =  $sp[2];
   }
   
   if(/\#define NPARAMS/) {
     @sparams = split; 
     $numParams =  $sparams[2];
   }

   for($n=0; $n< $numInputPorts; $n++){ 
     $strToMatch = $inDefinesBuilderDataBegin[0] . "$n" . $inDefinesBuilderDataEnd[0];
     if(/$strToMatch/){
       @ni = split; 
       $inportsName[$n] = "'$ni[2]'";	
     }

     $strToMatch = $inDefinesBuilderDataBegin[1] . "$n" . $inDefinesBuilderDataEnd[1];
     if(/$strToMatch/){
       @ni = split; 
       $inportsRow[$n] = "'$ni[2]'";	
     }

     $strToMatch = $inDefinesBuilderDataBegin[2] . "$n" . $inDefinesBuilderDataEnd[2];
     if(/$strToMatch/){
       @ni = split; 
       $inportsCol[$n] = "'$ni[2]'";	
     }
      
     $strToMatch = $inDefinesBuilderDataBegin[3] . "$n" . $inDefinesBuilderDataEnd[3];
     if(/$strToMatch/){
       @ni = split; 
       $inportsDataType[$n] = "'$ni[2]'";	
     }
     
     $strToMatch = $inDefinesBuilderDataBegin[4] . "$n" . $inDefinesBuilderDataEnd[4];
     if(/$strToMatch/){
       @ni = split; 
       $inportsComplex[$n] = "'$ni[2]'";	
     }
     
     $strToMatch = $inDefinesBuilderDataBegin[5] . "$n" . $inDefinesBuilderDataEnd[5];
     if(/$strToMatch/){
       @ni = split; 
       $inportsFrame[$n] = "'$ni[2]'";	
     }

     $strToMatch = $inDefinesBuilderDataBegin[6] . "$n" . $inDefinesBuilderDataEnd[6];
     if(/$strToMatch/){
       @ni = split; 
       $inportsBus[$n] = "'$ni[2]'";	
     }

     $strToMatch = $inDefinesBuilderDataBegin[7] . "$n" . $inDefinesBuilderDataEnd[7];
     if(/$strToMatch/){
       @ni = split; 
       $inportsBusname[$n] = "'$ni[2]'";
     }
     
     $strToMatch = $inDefinesBuilderDataBegin[8] . "$n" . $inDefinesBuilderDataEnd[8];
     if(/$strToMatch/){
       @ni = split; 
       $inportsDims[$n] = "'$ni[2]'";	
     }

     $strToMatch = $inDefinesBuilderDataBegin[9] . "$n" . $inDefinesBuilderDataEnd[9];
     if(/$strToMatch/){
       @ni = split; 
       $inportsIsSigned[$n] = "'$ni[2]'";	
     }
     $strToMatch = $inDefinesBuilderDataBegin[10] . "$n" . $inDefinesBuilderDataEnd[10];
     if(/$strToMatch/){
       @ni = split; 
       $inportsWordLength[$n] = "'$ni[2]'";	
     }
     $strToMatch = $inDefinesBuilderDataBegin[11] . "$n" . $inDefinesBuilderDataEnd[11];
     if(/$strToMatch/){
       @ni = split; 
       $inportsFractionLength[$n] = "'$ni[2]'";	
     }
     $strToMatch = $inDefinesBuilderDataBegin[12] . "$n" . $inDefinesBuilderDataEnd[12];
     if(/$strToMatch/){
       @ni = split; 
       $inportsFixPointScalingType[$n] = "'$ni[2]'";	
     }
     $strToMatch = $inDefinesBuilderDataBegin[13] . "$n" . $inDefinesBuilderDataEnd[13];
     if(/$strToMatch/){
       @ni = split; 
       $inportsSlope[$n] = "'$ni[2]'";	
     }
     $strToMatch = $inDefinesBuilderDataBegin[14] . "$n" . $inDefinesBuilderDataEnd[14];
     if(/$strToMatch/){
       @ni = split; 
       $inportsBias[$n] = "'$ni[2]'";	
     }
     
   }

   for($n=0; $n< $numOutputPorts; $n++){ 
     
     $strToMatch = $outDefinesBuilderDataBegin[0] . "$n" . $outDefinesBuilderDataEnd[0];
     if(/$strToMatch/){
       @ni = split; 
       $outportsName[$n] = "'$ni[2]'";	
     }

     $strToMatch = $outDefinesBuilderDataBegin[1] . "$n" . $outDefinesBuilderDataEnd[1];
     if(/$strToMatch/){
       @ni = split; 
       $outportsRow[$n] = "'$ni[2]'";	
     }

     $strToMatch = $outDefinesBuilderDataBegin[2] . "$n" . $outDefinesBuilderDataEnd[2];
     if(/$strToMatch/){
       @ni = split; 
       $outportsCol[$n] = "'$ni[2]'";	
     }
      
     $strToMatch = $outDefinesBuilderDataBegin[3] . "$n" . $outDefinesBuilderDataEnd[3];
     if(/$strToMatch/){
       @ni = split; 
       $outportsDataType[$n] = "'$ni[2]'";	
     }
     
     $strToMatch = $outDefinesBuilderDataBegin[4] . "$n" . $outDefinesBuilderDataEnd[4];
     if(/$strToMatch/){
       @ni = split; 
       $outportsComplex[$n] = "'$ni[2]'";	
     }
     
     $strToMatch = $outDefinesBuilderDataBegin[5] . "$n" . $outDefinesBuilderDataEnd[5];
     if(/$strToMatch/){
       @ni = split; 
       $outportsFrame[$n] = "'$ni[2]'";	
     }
     
    $strToMatch = $outDefinesBuilderDataBegin[6] . "$n" . $outDefinesBuilderDataEnd[6];
     if(/$strToMatch/){
       @ni = split; 
       $outportsBus[$n] = "'$ni[2]'";	
     }

     $strToMatch = $outDefinesBuilderDataBegin[7] . "$n" . $outDefinesBuilderDataEnd[7];
     if(/$strToMatch/){
       @ni = split; 
       $outportsBusname[$n] = "'$ni[2]'";	
     }

     $strToMatch = $outDefinesBuilderDataBegin[8] . "$n" . $outDefinesBuilderDataEnd[8];
     if(/$strToMatch/){
       @ni = split; 
       $outportsDims[$n] = "'$ni[2]'";	
     }

     $strToMatch = $outDefinesBuilderDataBegin[9] . "$n" . $outDefinesBuilderDataEnd[9];
     if(/$strToMatch/){
       @ni = split; 
       $outportsIsSigned[$n] = "'$ni[2]'";	
     }
     $strToMatch = $outDefinesBuilderDataBegin[10] . "$n" . $outDefinesBuilderDataEnd[10];
     if(/$strToMatch/){
       @ni = split; 
       $outportsWordLength[$n] = "'$ni[2]'";	
     }
     $strToMatch = $outDefinesBuilderDataBegin[11] . "$n" . $outDefinesBuilderDataEnd[11];
     if(/$strToMatch/){
       @ni = split; 
       $outportsFractionLength[$n] = "'$ni[2]'";	
     }
     $strToMatch = $outDefinesBuilderDataBegin[12] . "$n" . $outDefinesBuilderDataEnd[12];
     if(/$strToMatch/){
       @ni = split; 
       $outportsFixPointScalingType[$n] = "'$ni[2]'";	
     }
     $strToMatch = $outDefinesBuilderDataBegin[13] . "$n" . $outDefinesBuilderDataEnd[13];
     if(/$strToMatch/){
       @ni = split; 
       $outportsSlope[$n] = "'$ni[2]'";	
     }
     $strToMatch = $outDefinesBuilderDataBegin[14] . "$n" . $outDefinesBuilderDataEnd[14];
     if(/$strToMatch/){
       @ni = split; 
       $outportsBias[$n] = "'$ni[2]'";	
     }

   }
   
   for($n=0; $n< $numParams; $n++){ 
     
     $strToMatch = $ParameterDefinesBuilderDataBegin[0] . "$n" . $ParameterDefinesBuilderDataEnd[0];
     if(/$strToMatch/){
       @ni = split; 
       $parametersName[$n] = "'$ni[2]'";
     }

     $strToMatch = $ParameterDefinesBuilderDataBegin[1] . "$n" . $ParameterDefinesBuilderDataEnd[1];
     if(/$strToMatch/){
       @ni = split; 
       $parametersDataType[$n] = "'$ni[2]'";	
     }

     $strToMatch = $ParameterDefinesBuilderDataBegin[2] . "$n" . $ParameterDefinesBuilderDataEnd[2];
     if(/$strToMatch/){
       @ni = split; 
       $parametersComplex[$n] = "'$ni[2]'";	
     }
   }

 }
 
 if(/$beginStr/.../$endStr/){
   foreach $definesData (@DefinesWizardData){
     if(/$definesData\s*\.*/){
       ($var0, $var1, $var2) = /(\w+)\s+(\w+)\s+(.*)/;
       $assignMATLABVar =  "$assignMATLABVar\n @MATLABVariable[$i] =  '$var2';";
       $i++; 
     }
     
   }
 }
}  
close(HFile);
print "$assignMATLABVar\n";

# Send data to MATLAB
if ($sfbRev == '1.0') {
  # Update version 1.0 data to current data structure 
  $inportsName[0] = "u";
  $inportsRow[0] = "ad.SfunWizardData.InputPortWidth";
  $inportsDataType[0] = "real_T";
  $inportsCol[0] = "1";
  $inportsComplex[0] = "COMPLEX_NO";
  $inportsFrame[0] ="FRAME_NO";
  $inportsBus[0] ="off";
  $inportsBusname[0] = "";
  $inportsDims[0] = "1-D";
  $inportIsSigned[0]  = "";
  $inportWordLength[0]= "";
  $inportFractionLength[0]      = "";
  $inportFixPointScalingType[0] = "";
  $inportSlope[0]     = "";
  $inportBias[0]      = "";

  print " \nad.SfunWizardData.InputPorts.Name = { '$inportsName[0]' };\n";
  print " ad.SfunWizardData.InputPorts.Row = { $inportsRow[0] };\n";
  print " ad.SfunWizardData.InputPorts.Col = { '$inportsCol[0]' };\n ";
  print " ad.SfunWizardData.InputPorts.DataType = { '$inportsDataType[0]' };\n";
  print " ad.SfunWizardData.InputPorts.Complexity = { '$inportsComplex[0]' };\n";
  print " ad.SfunWizardData.InputPorts.Frame = { '$inportsFrame[0]' };\n";
  print " ad.SfunWizardData.InputPorts.Bus = { '$inportsBus[0]' };\n";
  print " ad.SfunWizardData.InputPorts.Busname = { '$inportsBusname[0]' };\n";
  print " ad.SfunWizardData.InputPorts.Dims = { '$inportsDims[0]' };\n";
  print " ad.SfunWizardData.InputPorts.outIsSigned = { '$inportIsSigned[0]' };\n";
  print " ad.SfunWizardData.InputPorts.outWordLength = { '$inportWordLength[0]' }\n";
  print " ad.SfunWizardData.InputPorts.outFractionLength = { '$inportFractionLength[0]' }\n";
  print " ad.SfunWizardData.InputPorts.outFixPointScalingType = { '$inportFixPointScalingType[0]' }\n";
  print " ad.SfunWizardData.InputPorts.outSlope = { '$inportSlope[0]' }\n";
  print " ad.SfunWizardData.InputPorts.outBias = { '$inportBias[0]' }\n";

  $outportsName[0]     = "y";
  $outportsRow[0]      = "ad.SfunWizardData.OutputPortWidth";
  $outportsDataType[0] = "real_T";
  $outportsCol[0]      = "1";
  $outportsComplex[0]  = "COMPLEX_NO";
  $outportsFrame[0]    ="FRAME_NO";
  $outportsBus[0] ="off";
  $outportsBusname[0] = "";
  $outportsDims[0]     = "1-D";
  $outportIsSigned[0]  = "";
  $outportWordLength[0]= "";
  $outportFractionLength[0]      = "";
  $outportFixPointScalingType[0] = "";
  $outportSlope[0]     = "";
  $outportBias[0]      = "";

  print " ad.SfunWizardData.OutputPorts.Name = { '$outportsName[0]' };\n";
  print " ad.SfunWizardData.OutputPorts.Row = { $outportsRow[0] };\n";
  print " ad.SfunWizardData.OutputPorts.Col = { '$outportsCol[0]' };\n ";
  print " ad.SfunWizardData.OutputPorts.DataType = { '$outportsDataType[0]' };\n";
  print " ad.SfunWizardData.OutputPorts.Complexity = { '$outportsComplex[0]' };\n";
  print " ad.SfunWizardData.OutputPorts.Frame = { '$outportsFrame[0]' };\n";
  print " ad.SfunWizardData.OutputPorts.Bus = { '$outportsBus[0]' };\n";
  print " ad.SfunWizardData.OutputPorts.Busname = { '$outportsBusname[0]' };\n";
  print " ad.SfunWizardData.OutputPorts.Dims = { '$outportsDims[0]' };\n";
  print " ad.SfunWizardData.OutputPorts.outIsSigned = { '$outportIsSigned[0]' };\n";
  print " ad.SfunWizardData.OutputPorts.outWordLength = { '$outportWordLength[0]' }\n";
  print " ad.SfunWizardData.OutputPorts.outFractionLength = { '$outportFractionLength[0]' }\n";
  print " ad.SfunWizardData.OutputPorts.outFixPointScalingType = { '$outportFixPointScalingType[0]' }\n";
  print " ad.SfunWizardData.OutputPorts.outSlope = { '$outportSlope[0]' }\n";
  print " ad.SfunWizardData.OutputPorts.outBias = { '$outportBias[0]' }\n";

} else {

 ##############
 # inputs     #
 ##############
  print "$inPortsStruct[0] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsName[$n]";
  }
  print "};\n";
  print "$inPortsStruct[1] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsRow[$n]";
  }
  print "};\n";

  print "$inPortsStruct[3] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsDataType[$n]";
  }
  print "};\n";

  print "$inPortsStruct[2] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsCol[$n]";
  }
  print "};\n";

  print "$inPortsStruct[4] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsComplex[$n]";
  }
  print "};\n";

  print "$inPortsStruct[5] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsFrame[$n]";
  }
  print "};\n";

  print "$inPortsStruct[6] = {";
  for($n=0; $n< $numInputPorts; $n++) {
      if($inportsBus[$n] eq "'1'"){
          print " 'on'";
      }else{
          print " 'off'";
      }
  }
  print "};\n";

  print "$inPortsStruct[7] = {";
  for($n=0; $n< $numInputPorts; $n++) {
      if($inportsBusname[$n] eq ""){
          print " ''";
      }else{
          print " $inportsBusname[$n]";
      }
  }
  print "};\n";

  print "$inPortsStruct[8] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsDims[$n]";
  }
  print "};\n";

  print "$inPortsStruct[9] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsIsSigned[$n]";
  }
  print "};\n";

  print "$inPortsStruct[10] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsWordLength[$n]";
  }
  print "};\n";

  print "$inPortsStruct[11] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsFractionLength[$n]";
  }
  print "};\n";

  print "$inPortsStruct[12] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsFixPointScalingType[$n]";
  }
  print "};\n";

  print "$inPortsStruct[13] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsSlope[$n]";
  }
  print "};\n";

  print "$inPortsStruct[14] = {";
  for($n=0; $n< $numInputPorts; $n++) {
    print " $inportsBias[$n]";
  }
  print "};\n";

 ##############
 # outputs    #
 ##############
  print "$outPortsStruct[0] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsName[$n]";
  }
  print "};\n";
  print "$outPortsStruct[1] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsRow[$n]";
  }
  print "};\n";

  print "$outPortsStruct[3] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsDataType[$n]";
  }
  print "};\n";

  print "$outPortsStruct[2] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsCol[$n]";
  }
  print "};\n";

  print "$outPortsStruct[4] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsComplex[$n]";
  }
  print "};\n";

  print "$outPortsStruct[5] = {";
  for($n=0; $n < $numOutputPorts; $n++) {
    print " $outportsFrame[$n]";
  }
  print "};\n";

  print "$outPortsStruct[6] = {";
  for($n=0; $n < $numOutputPorts; $n++) {
      if($outportsBus[$n] eq "'1'"){      
          print " 'on'";
      }else{
          print " 'off'";
      }
  }
  print "};\n";
  
  
  print "$outPortsStruct[7] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
      if($outportsBusname[$n] eq ""){
          print " ''";
      }else{
          print " $outportsBusname[$n]";
      }
  }
  print "};\n";

print "$outPortsStruct[8] = {";
  for($n=0; $n < $numOutputPorts; $n++) {
    print " $outportsDims[$n]";
  }
  print "};\n";


  print "$outPortsStruct[9] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsIsSigned[$n]";
  }
  print "};\n";

  print "$outPortsStruct[10] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsWordLength[$n]";
  }
  print "};\n";

  print "$outPortsStruct[11] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsFractionLength[$n]";
  }
  print "};\n";

  print "$outPortsStruct[12] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsFixPointScalingType[$n]";
  }
  print "};\n";

  print "$outPortsStruct[13] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsSlope[$n]";
  }
  print "};\n";

  print "$outPortsStruct[14] = {";
  for($n=0; $n< $numOutputPorts; $n++) {
    print " $outportsBias[$n]";
  }
  print "};\n";

 ##############
 # parameters #
 ##############
  print "$parametersStruct[0] = {";
  for($n=0; $n< $numParams; $n++) {
    print " $parametersName[$n]";
  }
  print "};\n";
  print "$parametersStruct[1] = {";
  for($n=0; $n< $numParams; $n++) {
    print " $parametersDataType[$n]";
  }
  print "};\n";

  print "$parametersStruct[2] = {";
  for($n=0; $n< $numParams; $n++) {
    print " $parametersComplex[$n]";
  }
  print "};\n";
}









