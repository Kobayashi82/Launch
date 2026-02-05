Set Staged to False.
//Calculate pitch based in altitud

/////////////////////////
/////// PRE-START ///////
/////////////////////////

ClearScreen. Set Config:IPU to 500. Set Ship:Control:PilotMainThrottle to 0. If Ship:Status = "Orbiting" { InOrbit(). } GetLastEngine(). If LastEngineStage = 999 NoEngines().
If Body:Atm:Height > 0 { Set TargetApoapsis to Body:Atm:Height + 10000. } Else { Set TargetApoapsis to Round(Body:Radius / 5). }
If Floor(ABS(Latitude)) > 0 { Set TargetInclination to Floor(ABS(Latitude)). } Else { Set TargetInclination to 0. }
Set AutoCircularize to "Yes". Set AutoStage to "Yes". Set DVSpent to 0. Set DVSpentTime to Time:Seconds. Set InitialTWR to 1.4. Set ApoTime to 60. Set RCSON to False. Set SelectedTarget to HasTarget. Set PTarget to "None". Set TTarget to "None". Set DirectTarget to False. Set MyTargets to GetTargetList(). Set MyTargetsIndex to 0.

Print "                 LAUNCH AUTOPILOT                 " at (0, 2).
Print "  .=============================================. " at (0, 4).
Print "  | Apoapsis:            | Inclination:         | " at (0, 5).
Print "  |=============================================| " at (0, 6).
Print "  .=============================================. " at (0, 8).
Print "  | (A) Auto-Stage:      | (C) Circularize:     | " at (0, 9).
Print "  |---------------------------------------------| " at (0, 10).
Print "  | (T) Target:                                 | " at (0, 11).
Print "  |=============================================| " at (0, 12).
Print "  By: Kobayashi " at (30, 30).

Set StartProgram to False. Until StartProgram = True {
  If PTarget <> "None" and SelectedTarget = False and TargetExist(PTarget) = True {
    Set SelectedTarget to True.
    Print "                      TARGET                      " at (0, 14).
    Print "  .=============================================. " at (0, 16).
    Print "  | Apoapsis:            | Inclination:         | " at (0, 17).
    Print "  |=============================================| " at (0, 18).
  } Else If PTarget = "None" and SelectedTarget = True {
    Set SelectedTarget to False.
    Print Spacer(0,52) at (0, 14).
    Print Spacer(0,52) at (0, 16).
    Print Spacer(0,52) at (0, 17).
    Print Spacer(0,52) at (0, 18).
  }
  If TargetExist(PTarget) = True {
    Set TTarget to Vessel(PTarget).
    Set TApoapsis to FormatDistance(TTarget:Apoapsis).		        Print TApoapsis + Spacer(TApoapsis:Length, 8) at (17, 17).
    Set TInclination to Round(TTarget:Orbit:Inclination, 3) + "".         Print TInclination + Spacer(TInclination:Length, 7) at (40, 17).
  }
  Set PApoapsis to FormatDistance(TargetApoapsis).			Set PInclination to TargetInclination + "".
  Print PApoapsis + Spacer(PApoapsis:Length, 8) at (17, 5). 		Print PInclination + Spacer(PInclination:Length, 5) at (43, 5).
  Print AutoStage + Spacer(AutoStage:Length, 4) at (21, 9).		Print AutoCircularize + Spacer(AutoCircularize:Length, 4) at (44, 9).
  Set PTarget to MyTargets[MyTargetsIndex].                              Print PTarget + Spacer(PTarget:Length, 32) at (16, 11).
  If Terminal:Input:HasChar { Set ch to Terminal:Input:GetChar().
    If ch = Terminal:Input:UpCursorOne { If TargetApoapsis < 5000000 Set TargetApoapsis to TargetApoapsis + 1000. }
    If ch = Terminal:Input:DownCursorOne { If Body:Atm:Exists = True { If TargetApoapsis > Body:Atm:Height + 5000 Set TargetApoapsis to TargetApoapsis - 1000. } Else { If TargetApoapsis > 0 Set TargetApoapsis to TargetApoapsis - 1000. }}
    If UnChar(ch) = 112 { If TargetApoapsis < 5000000 Set TargetApoapsis to TargetApoapsis + 10000. If TargetApoapsis > 5000000 Set TargetApoapsis to 5000000. }
    If UnChar(ch) = 80 { If Body:Atm:Exists = True { If TargetApoapsis > Body:Atm:Height + 5000 Set TargetApoapsis to TargetApoapsis - 10000. If TargetApoapsis < Body:Atm:Height + 5000 Set TargetApoapsis to Body:Atm:Height + 5000. } Else { If TargetApoapsis > 1000 Set TargetApoapsis to TargetApoapsis - 10000. If TargetApoapsis < 1000 Set TargetApoapsis to 1000. }}
    If ch = Terminal:Input:RightCursorOne { If ABS(TargetInclination + 1) < Floor(ABS(Latitude)) Set TargetInclination to (TargetInclination + 1) * -1. If ABS(TargetInclination + 1) <= 181 - Ceiling(ABS(Latitude)) Set TargetInclination to TargetInclination + 1. }
    If ch = Terminal:Input:LeftCursorOne { If ABS(TargetInclination - 1) < Floor(ABS(Latitude)) Set TargetInclination to (TargetInclination - 1) * -1. If ABS(TargetInclination -1) <= 181 - Ceiling(ABS(Latitude)) Set TargetInclination to TargetInclination - 1. }
    If ch = "c" If AutoCircularize = "Yes" { Set AutoCircularize to "No". } Else { Set AutoCircularize to "Yes". }
    If ch = "a" If AutoStage = "Yes" { Set AutoStage to "No". } Else { Set AutoStage to "Yes". }
    If UnChar(ch) = 116 { If MyTargetsIndex = MyTargets:Length -1 { Set MyTargetsIndex to 0. } Else { Set MyTargetsIndex to MyTargetsIndex + 1. } Set PTarget to MyTargets[MyTargetsIndex]. }
    If UnChar(ch) = 84 { If MyTargetsIndex = 0 { Set MyTargetsIndex to MyTargets:Length -1. } Else { Set MyTargetsIndex to MyTargetsIndex - 1. } Set PTarget to MyTargets[MyTargetsIndex]. }
    If ch = Terminal:Input:Enter { If TTarget <> "None" { Set DirectTarget to True. Set PApoapsis to TTarget:Apoapsis. Set PInclination to TTarget:Orbit:Inclination. } Set StartProgram to True. }
  } If Terminal:Input:HasChar Terminal:Input:Clear.
}

///////////////////////////
/////// INFORMATION ///////
///////////////////////////

If AutoStage = "Yes" { If Ship:Status <> "Landed" and Ship:Status <> "Splashed" { Set Mode to "calculating". } Else { Set Mode to "launching". }} Else { Set Mode to "waiting to launch". }
Print "                 LAUNCH AUTOPILOT                 " at (0, 2).
Print "  .=============================================. " at (0, 4).
Print "  | MODE:                                       | " at (0, 5).
Print "  |---------------------------------------------| " at (0, 6).
Print "  | Stage DV:            | Total DV:            | " at (0, 7).
Print "  |=============================================| " at (0, 8).
Print Spacer(0,52) at (0, 9).
Print "                       ORBIT                      " at (0, 10).
Print "  .=============================================. " at (0, 12).
Print "  | Apoapsis:            | Inclination:         | " at (0, 13).
Print "  |---------------------------------------------| " at (0, 14).
Print "  | Periapsis:           | Altitude:            | " at (0, 15).
Print "  |=============================================| " at (0, 16).
Print Spacer(0,52) at (0, 17).
Print "                       TARGET                     " at (0, 18).
Print "  .=============================================. " at (0, 20).
Print "  | Name:                                       | " at (0, 21).
Print "  |---------------------------------------------| " at (0, 22).
Print "  | Apoapsis:            | Inclination:         | " at (0, 23).
Print "  |---------------------------------------------| " at (0, 24).
Print "  | Distance:            | :                    | " at (0, 25).
Print "  |=============================================| " at (0, 26).
Print "  By: Kobayashi " at (30, 30).
PrintInfo().

Function PrintInfo {
  Print Mode + Spacer(Mode:Length, 36) at (11, 5).
  Set PStageDV to GetDV("Stage"). 				        Print PStageDV + Spacer(PStageDV:Length, 9) at (16, 7).
  Set PSpentDV to GetDV("Spent").                         	        Print PSpentDV + Spacer(PSpentDV:Length, 9) at (38, 7).

  Set CApoapsis to FormatDistance(Apoapsis, 2).			        Print CApoapsis + Spacer(CApoapsis:Length, 10) at (15, 13).
  Set CPeriapsis to FormatDistance(Periapsis).			        Print CPeriapsis + Spacer(CPeriapsis:Length, 10) at (15, 15).
  Set CInclination to Round(Ship:Orbit:Inclination, 2) + "".            Print CInclination + Spacer(CInclination:Length, 7) at (40, 13).
  Set CAltitude to FormatDistance(Altitude, 2).			        Print CAltitude + Spacer(CAltitude:Length, 7) at (40, 15).

  If SelectedTarget = True {
    Print TTarget:Name + Spacer(TTarget:Name:Length, 37) at (10, 21).    
    Set TApoapsis to FormatDistance(TTarget:Apoapsis, 2).	        Print TApoapsis + Spacer(TApoapsis:Length, 10) at (15, 23).
    Set TInclination to Round(TTarget:Orbit:Inclination, 3) + "".       Print TInclination + Spacer(TInclination:Length, 7) at (17, 25).
    Set TDistance to FormatDistance(TTarget:Distance, 2).               Print TDistance + Spacer(TDistance:Length, 9) at (39, 23).
    Set CEmpty to "".                                                   Print CEmpty + Spacer(CEmpty:Length, 6) at (41, 25).
  }
}

/////////////////////////
/////// MAIN LOOP ///////
/////////////////////////

Set LaunchCompleted to False. Set RunMode to 0. Set UpdateSettings to True. Set Staging to False. SAS Off. Set ShipHeight to GetShipHeight(Ship). Set SteerPitch to 0.
Set TKp to 0.1. Set TKi to 0.01. Set TKd to 0.05. Set Tt0 to Time:Seconds. Lock TP to ApoTime - ETA:Apoapsis. Set TI to 0. Set TP0 to TP. Set Tdth to 0. Set TD to 0. Lock Tdth to TKp * TP + TKi * TI + TKd * TD.
When RunMode = 1 Then { If Body:Atm:Height > 0 { Set Tdt to Time:Seconds - Tt0. If Tdt > 0 { If ETA:Apoapsis > ETA:Periapsis { Set TP to ApoTime. } Else { Set TP to ApoTime - ETA:Apoapsis. } If ABS(TP) < 2 { Set TI to TI + TP * Tdt. } Else { Set TI to 0. } Set TD to (TP - TP0) / Tdt. If Staging = False Set Throt to Min(TWR(MaxTWR), Max(0.1, Throt + Tdth)). Set Tt0 to Time:Seconds. Set TP0 to TP. } If Apoapsis < TargetApoapsis { Preserve. } Else { Set Throt to 0. }} Else { If Apoapsis < TargetApoapsis { Set Throt to TWR(Max((TargetApoapsis - Apoapsis) / 1000, 1)). Preserve. } Else { Set Throt to 0. }}}
If AutoStage = "Yes" { Until AvailableThrust > 0 { Stager(). If AvailableThrust = 0 and LastEngineStage = Stage:Number NoEngines(). }} PrintInfo(). Set LaunchAzimuth to GetLaunchAzimuth(TargetApoapsis, TargetInclination). Set SteerHeading to LaunchAzimuth. Set TurnStart to Round((Altitude + (Altitude - Ship:GeoPosition:TerrainHeight)) / 10) * 10. Set MinPitch to 15.
If Body:Atm:Height > 0 { If TWR("MAX"):ToNumber < InitialTWR Set InitialTWR to TWR("MAX"):ToNumber. Set TurnEnd to (0.128 * Body:Atm:Height * Min(TWR("MAX"):ToNumber, InitialTWR) + 0.6 * Body:Atm:Height). Set TurnExponent to Max(1 / (2.5 * Min(TWR("MAX"):ToNumber, InitialTWR) - 1.7), 0.25). }
Lock MaxTWR to InitialTWR + ((1 - Body:Atm:AltitudePressure(Altitude)) * 1.0). Set Throt to TWR(MaxTWR).
Set SteerTo to Up. Lock Steering to SteerTo. Lock Throttle to Throt. Wait Until TWR("CUR"):ToNumber > 1.1.
If AutoStage = "Yes" { PrepareShip("Clamp", True). If Body:Atm:Exists { When Altitude > Body:Atm:Height * 0.80 Then { PrepareShip("Fairing", True). PrepareShip("LES", True). }}} On Abort { Set LaunchCompleted to True. Set Mode to "launch Aborted". }
If Altitude - ShipHeight > 100 Set RunMode to 1.
If Body:Atm:Height > 0 and Apoapsis >= TargetApoapsis { If Altitude < Body:Atm:Height { Set RunMode to 2. } Else { If AutoCircularize = "Yes" { Set RunMode to 3. } Else { Set LaunchCompleted to True. Set Mode to "launch completed". PrintInfo. }}}
If Body:Atm:Height = 0 and Apoapsis >= TargetApoapsis { If AutoCircularize = "Yes" { Set RunMode to 3. } Else { Set LaunchCompleted to True. Set Mode to "launch completed". PrintInfo. }}
If Apoapsis >= TargetApoapsis and ETA:Periapsis < ETA:Apoapsis { ClearScreen. InDeOrbit(). }
Until LaunchCompleted = True { If SAS = True SAS Off.

			//////////////////////////////
			/////// VERICAL ASCENT ///////
			//////////////////////////////
  If RunMode = 0 {
    If UpdateSettings = True { Set UpdateSettings to False. Set Mode to "initial ascent". Set ShipHeight to GetShipHeight(Ship). If Body:Atm:Height = 0 Set TurnStart to Altitude + 100. }
    If Body:Atm:Height > 0 { Set Throt to TWR(MaxTWR). } Else { Set Throt to TWR(Max((TargetApoapsis - Apoapsis) / 1000, 1)). }
    If Alt:Radar - ShipHeight > 10 { Gear On. Gear Off. } If Altitude - ShipHeight > TurnStart { Set RunMode to 1. Set UpdateSettings to True. }
  }
			/////////////////////////////////
			/////// ASCENT TRAJECTORY ///////
			/////////////////////////////////  
  If RunMode = 1 {
    If UpdateSettings = True { Set UpdateSettings to False. Set Mode to "ascent trajectory". Set BrokeApoTime to False. If Body:Atm:Height = 0 { Set TurnEnd to TargetApoapsis. Set TurnExponent to 0.13. }}
    Set SteerPitch to Max(90 - (((Max(Altitude - TurnStart, 1)) / (TurnEnd - TurnStart))^TurnExponent * 90), MinPitch). If Body:Atm:Height > 0 { If BrokeApoTime and ETA:Apoapsis < ApoTime { Set SteerPitch to SteerPitch + (ApoTime - ETA:Apoapsis). } Else If ETA:Apoapsis > ApoTime and Not BrokeApoTime { Set BrokeApoTime to True. }} If ABS(Ship:OBT:Inclination - ABS(TargetInclination)) > 2 { Set SteerHeading to LaunchAzimuth. } Else { If TargetInclination >= 0 { If VAng(VXCL(Ship:Up:Vector, Ship:Facing:Vector), Ship:North:Vector) <= 90 { Set SteerHeading to (90 - TargetInclination) - 2 * (ABS(TargetInclination) - Ship:OBT:Inclination). } Else { Set SteerHeading to (90 - TargetInclination) + 2 * (ABS(TargetInclination) - Ship:OBT:Inclination). }} Else { Set SteerHeading to (90 - TargetInclination) + 2 * (ABS(TargetInclination) - Ship:OBT:Inclination). }} Set AscentSteer to Heading(SteerHeading, SteerPitch).
    If Ship:Q > 0 { Set AngleLimit to Max(3, Min(90, 5 * LN(0.9 / Ship:Q))). } Else { Set AngleLimit to 90. } Set AngleToPrograde to VAng(Ship:SRFPrograde:Vector, AscentSteer:Vector). If AngleToPrograde > AngleLimit { Set AscentSteerLimited to (AngleLimit / AngleToPrograde * (AscentSteer:Vector:Normalized - Ship:SRFPrograde:Vector:Normalized)) + Ship:SRFPrograde:Vector:Normalized. Set AscentSteer to AscentSteerLimited:Direction. } Set SteerTo to AscentSteer.
    If Body:Atm:Height > 0 and Apoapsis >= TargetApoapsis { If Altitude < Body:Atm:Height { Set RunMode to 2. Set UpdateSettings to True. } Else { If AutoCircularize = "Yes" { Set RunMode to 3. Set UpdateSettings to True. } Else { Set LaunchCompleted to True. Set Mode to "launch completed". }} Set Throt to 0. Set SteerPitch to 0. }
    If Body:Atm:Height = 0 and Apoapsis >= TargetApoapsis { If AutoCircularize = "Yes" { Set RunMode to 3. Set UpdateSettings to True. } Else { Set LaunchCompleted to True. Set Mode to "launch completed". } Set Throt to 0. Set SteerPitch to 0. }
    UseRCS(Heading(SteerHeading, SteerPitch)).
  }
			//////////////////////////////////////////
			/////// COASTING OUT OF ATMOSPHERE ///////
			//////////////////////////////////////////  
  If RunMode = 2 {
    If UpdateSettings = True { Set UpdateSettings to False. Set Mode to "coast out of atmosphere". Set SteerTo to Ship:SRFPrograde. }
    If Altitude > Body:Atm:Height { If AutoCircularize = "Yes" { Set RunMode to 3. Set UpdateSettings to True. } Else { Set LaunchCompleted to True. Set Mode to "launch completed". } If Warp > 0 Set Warp to 0. Wait 1. }
    If Apoapsis < TargetApoapsis { Set Throt to TWR(0.5). } Else { Set Throt to 0. }
    UseRCS(Heading(SteerHeading, SteerPitch)).
  }
			///////////////////////////
			/////// CIRCULARIZE ///////
			///////////////////////////
  If RunMode = 3 {
    If UpdateSettings = True { Set UpdateSettings to False. Circularize. ExecNode(). }
    If RunExecNode = "Completed" or RunExecNode = "Failed" { Set LaunchCompleted to True. Set Mode to "launch completed". PrintInfo(). }
  }

  If AutoStage = "Yes" and GetDV("Stage", True) = 0 {
    If AvailableThrust = 0 {
      If LastEngineStage < Stage:Number {
        Set Staging to True. Set Throt to 0. PrintInfo(). Wait 0.5. Stage. PrintInfo(). Wait 1. Set Staging to False.
      } Else {
        Set AutoStage to "No".
      }
    } Else {
      If Staged = False { Stage. Set Staged to True. }
    }
  }

  PrintInfo(). Wait 0.
}
If HasNode = True Remove NextNode.
If Warp > 0 Set Warp to 0. Wait 2. Unlock Thottle. Unlock Steering. SAS On. Wait 3. SAS Off. RCS Off. Wait 2. ClearScreen. If Core:HasAction("Close Terminal") = True Core:DoAction("Close Terminal", True).

// .=====================================================================.
// |                              FUNCTIONS                              |
// |=====================================================================|

Function UseRCS{
  Parameter hWork is 0. Parameter vMax is 1. Parameter vMin is .25.
  If Not RCSON and VAng(Ship:Facing:Vector, hWork:Vector) > ABS(vMax) { RCS On. Set RCSON to True. }
  If RCSON and VAng(Ship:Facing:Vector, hWork:Vector) < ABS(vMin) { RCS Off. Set RCSON to False. }
}

// .=====.
// | TWR |
// |=====|

Function TWR {
  Parameter TWRMode, cAltitude is Altitude.	// "CUR" -> Current | "MAX" -> Maximum | [Number] -> Set TWR
  If TWRMode = "CUR" or TWRMode = "MAX" {
    Set mThrust to 0. Set cThrust to 0. List Engines in EngList.
    For Eng in EngList { If Eng:Name:Contains("decoupler") = False and Eng:Name:Contains("heatshield") = False {Set cThrust to cThrust + Eng:Thrust. If Eng:Ignition = True Set mThrust to mThrust + Eng:AvailableThrust. }}
    Set cThrust to Round(cThrust / ((Body:Mu / (Body:Radius + cAltitude)^2) * Mass), 2) + "". If cThrust:Contains(".") = True { If (cThrust + ""):Length - cThrust:Find(".") = 2 Set cThrust to cThrust + "0". } Else { Set cThrust to cThrust + ".00". }
    Set mThrust to Round(mThrust / ((Body:Mu / (Body:Radius + cAltitude)^2) * Mass), 2) + "". If mThrust:Contains(".") = True { If (mThrust + ""):Length - mThrust:Find(".") = 2 Set mThrust to mThrust + "0".  } Else { Set mThrust to mThrust + ".00". }
    If TWRMode = "CUR" Return cThrust. If TWRMode = "MAX" Return mThrust.
  } Else { Return (TWRMode * (Constant:G * ((Mass * Body:Mass) / ((cAltitude + Body:Radius)^2)))) / (AvailableThrust + 0.001). }
}

// .=============.
// | GET DELTA-V |
// |=============|

Function GetDV {
  Parameter DVMode. Parameter Numerical is False.
  If DVMode = "Spent" {
    // Set DVSpentTime to Time:Seconds. Set DVSpent to 0. (at the begining)
    Local IgnoreEngine to False.    If DVSpentTime < Time:Seconds {      Set cThrust to 0. List Engines in EngList.      For Eng in EngList {
        Set IgnoreEngine to False.
        If Eng:Name:Contains("decoupler") = True Set IgnoreEngine to True.
        If Eng:Name:Contains("heatshield") = True Set IgnoreEngine to True.
        If IgnoreEngine = False and Eng:Ignition = True Set cThrust to cThrust + Eng:Thrust.
      }      Set DVSpent to DVSpent + ((cThrust / Mass) * (Time:Seconds - DVSpentTime)).      Set DVSpentTime to Time:Seconds.    }
    If Numerical = True { Return DVSpent. } Else { Return Round(DVSpent) + " m/s". }  } Else If DVMode = "Stage" {
      If AvailableThrust > 0 {
        Set FuelMass to Stage:LiquidFuel * 0.005 + Stage:Oxidizer * 0.005 + Stage:SolidFuel * 0.0075 + Stage:MonoPropellant * 0.004.
        Set RemoveFuelMass to 0. Set PartList to Ship:Parts. For Part in PartList{
          If Part:Stage >= Stage:Number -1 and Part:Tag = "nostage" {
            For Res in Part:Resources {
              If Res:Name = "LiquidFuel" Set RemoveFuelMass to RemoveFuelMass + Res:Amount * 0.005.
              If Res:Name = "Oxidizer" Set RemoveFuelMass to RemoveFuelMass + Res:Amount * 0.005.
              If Res:Name = "SolidFuel" Set RemoveFuelMass to RemoveFuelMass + Res:Amount * 0.0075.
              If Res:Name = "MonoPropellant" Set RemoveFuelMass to RemoveFuelMass + Res:Amount * 0.004.
            }
          }
        }
        Set FuelMass to Max(FuelMass - RemoveFuelMass, 0).        Set cmThrust to 0. Set mDot to 0. List Engines in EngList.        For Eng in EngList {
          Set IgnoreEngine to False.
          If Eng:Name:Contains("decoupler") = True Set IgnoreEngine to True.
          If Eng:Name:Contains("heatshield") = True Set IgnoreEngine to True.
          If Eng:Tag = "nostage" Set IgnoreEngine to True.
          If IgnoreEngine = False and Eng:Ignition = True { Set cmThrust to cmThrust + Eng:AvailableThrust. If Eng:ISP <> 0 Set mDot to mDot + (Eng:AvailableThrust / Eng:ISP). }
        }        If mDot <> 0 { Set AvgISP to cmThrust / mDot. } Else { Set AvgISP to 0. }
        Local StageDV to AvgIsp * (Body("Kerbin"):Mu / Body("Kerbin"):Radius^2) * LN(Mass / (Mass - FuelMass)).
        For Eng in EngList { If IgnoreEngine = False and Eng:Stage = Stage:Number and Eng:FlameOut = True Set StageDV to 0. }        
        If Numerical = True { Return StageDV. } Else { Return Round(StageDV) + " m/s". }      } Else { If Numerical = True { Return 0. } Else { Return "0 m/s". }    }
  }}

// .========.
// | STAGER |
// |========|

Function Stager { Parameter cWait is 1.
  Set LastEngineStage to 999. List Engines in EnginesList.
  If EnginesList:Length > 0 {
    For Eng in EnginesList {
      If Eng:Stage = Stage:Number Eng:Activate.
      If Eng:Stage < LastEngineStage Set LastEngineStage to Eng:Stage.
    }
  }
  Wait 0. If AvailableThrust = 0 and LastEngineStage < Stage:Number Stage. Wait cWait.
}
Function GetLastEngine { Set LastEngineStage to 999. List Engines in EnginesList. If EnginesList:Length > 0 { For Eng in EnginesList { If Eng:Stage < LastEngineStage Set LastEngineStage to Eng:Stage. }}}

// .=================.
// | FORMAT DISTANCE |
// |=================|

Function FormatDistance { Parameter Value, Decimals is 0.
  If Value < 1000 { Return Max(Round(Value), 0) + " m.".
  } Else If Value < 100000 {
    Local Result to Max(Round(Value / 1000, Decimals), 0) + "".
    If Decimals > 0 { If Result:Contains(".") = False { Return Result + "." + "0000000000":SubString(0, Decimals) + " km.". } Else { Return Result + " km.". }} Else { Return Result + " km.". }
  } Else { Return Max(Round(Value / 1000), 0) + " km.". }
}

// .=============.
// | FORMAT TIME |
// |=============|

Function FormatTime { Parameter PSeconds. Parameter Parentesis is True. Parameter NoNegative is False. If NoNegative = True and PSeconds < 0 Set PSeconds to 0.
  Set d to Floor(PSeconds / 21600). Set h to Floor((PSeconds - 21600 * d) / 3600). Set m to Floor((PSeconds - 3600 * h - 21600 * d) / 60).
  If m < 10 { Set sm to "0" + m. } Else { Set sm to m. } Set s to Round(PSeconds) - m * 60 - h * 3600 - 21600 * d. If s < 10 { Set ss to "0" + s. } Else { Set ss to s. }
  If d = 1 { Set fdays to d + " day - ". } Else { Set fdays to d + " days - ". } Set h to h + ((d * 21600) / 3600). Set d to 0.
  If Parentesis = True { If d = 0 { Return "(" + h + ":" + sm + ":" + ss + ")". } Else { Return "(" + fdays + h + ":" + sm + ":" + ss + ")". }} Else { If d = 0 { Return h + ":" + sm + ":" + ss. } Else { Return fdays + h + ":" + sm + ":" + ss. }}
}

// .====================.
// | GET LAUNCH AZIMUTH |
// |====================|

Function GetLaunchAzimuth { Parameter cApoapsis, cInclination.
  Set cLaunchAzimuth to ArcTan(((SQRT(Body:Mu / (cApoapsis + Body:Radius))) * Sin((ArcSin(Max(Min(Cos(cInclination) / Cos(Ship:GeoPosition:Lat), 1), -1)))) - (6.2832 * Body:Radius / Body:RotationPeriod)) / ((SQRT(Body:Mu / (cApoapsis + Body:Radius))) * Cos((ArcSin(Max(Min(Cos(cInclination) / Cos(Ship:GeoPosition:Lat), 1), -1)))))).
  If cInclination < 0 { Set cLaunchAzimuth to 180 - cLaunchAzimuth.} Return cLaunchAzimuth.
}

// .=================.
// | GET SHIP HEIGHT |
// |=================|

Function GetShipHeight { Parameter cVessel. Set LowestPart to 0. Set HighestPart to 0. Lock R3 to cVessel:Facing:ForeVector. Set PartList to cVessel:Parts.
  For Part in PartList{ Set V to Part:Position. Set CurrentPart to R3:X * V:X + R3:Y * V:Y + R3:Z * V:Z. If CurrentPart > HighestPart Set HighestPart to CurrentPart. Else If CurrentPart < LowestPart Set LowestPart to CurrentPart. }
  Return HighestPart - LowestPart.
}

// .==============.
// | PREPARE SHIP |
// |==============|

Function PrepareShip {
  Parameter Name, cMode.
  If cMode = True {
    If Name = "Clamp" { List Parts in PartsList. For Part in PartsList { For Module in Part:Modules { If Module = "LaunchClamp" { If Part:GetModule("LaunchClamp"):HasEvent("Release Clamp") = True Part:GetModule("LaunchClamp"):DoEvent("Release Clamp"). }}}}
    If Name = "Fairing" { List Parts in PartsList. For Part in PartsList { For Module in Part:Modules { If Module = "ModuleProceduralFairing" { If Part:GetModule("ModuleProceduralFairing"):HasEvent("Deploy") = True Part:GetModule("ModuleProceduralFairing"):DoEvent("Deploy"). } If Module = "ProceduralFairingDecoupler" { If Part:GetModule("ProceduralFairingDecoupler"):HasEvent("Jettison") = True Part:GetModule("ProceduralFairingDecoupler"):DoEvent("Jettison"). }}}}
    If Name = "LES" { List Parts in PartsList. For Part in PartsList { For Module in Part:Modules { If Module = "ModulePebkacLesController2" { If Part:GetModule("ModulePebkacLesController2"):HasEvent("Jettison Les") = True Part:GetModule("ModulePebkacLesController2"):DoEvent("Jettison Les"). }}}}
  }
}

// .=============.
// | CIRCULARIZE |
// |=============|

Function Circularize { Add Node(Time:Seconds + ETA:Apoapsis, 0, 0, (SQRT(Velocity:Orbit:MAG^2 + (Body:Mu * (2 / (Body:Radius + Apoapsis) - 2 / (Body:Radius + Altitude) + 1 / Ship:Orbit:SemiMajorAxis - 1 / ((Apoapsis + 2 * Body:Radius + Apoapsis) / 2))))) - (SQRT( Velocity:Orbit:MAG^2 + 2 * Body:Mu * (1 / (Body:Radius + Apoapsis) - 1 / (Body:Radius + Altitude))))). }

// .==============.
// | EXECUTE NODE |
// |==============|

Function ExecNode { Parameter cAutoWarp is "Yes".
  Local Ens to List(). Local Ens_Thrust to 0.  Local Ens_Isp to 0. Local BV is NextNode:BurnVector.
  List Engines in MyEngines. For En in MyEngines { If En:Name:Contains("decoupler") = False and En:Name:Contains("heatshield") = false { If En:Ignition = True and En:Flameout = False Ens:Add(En). }} For En in Ens { Set Ens_Thrust to Ens_Thrust + En:AvailableThrust. Set Ens_Isp to Ens_Isp + En:Isp. }
  Set StartTime to Time:Seconds + NextNode:ETA - ((Body:Mu / Body:Radius^2) * (Mass * 1000) * (Ens_Isp / Ens:Length) * (1 - Constant:E^(-NextNode:BurnVector:MAG/((Body:Mu / Body:Radius^2) * (Ens_Isp / Ens:Length)))) / (Ens_Thrust * 1000)) / 2.
  If Warp > 0 Set Warp to 0. Set RunExecNode to "". Lock Steering to NextNode:BurnVector.
  When VAng(Facing:Vector, NextNode:BurnVector) < 2 Then { Set RunExecNode to "Start". If cAutoWarp = "Yes" { Set WarpMode to "Rails". WarpTo(StartTime - 30). }}
  When GetDV("Stage", True) =  0 Then { When GetDV("Stage", True) > 0 Then { List Engines in MyEngines. For En in MyEngines { If Eng:Ignition = True and En:Flameout = False Ens:Add(En). } For En in Ens { Set Ens_Thrust to Ens_Thrust + En:AvailableThrust. Set Ens_Isp to Ens_Isp + En:Isp. }}} 
  When RunExecNode = "Start" Then {
    If Time:Seconds >= StartTime { Set Mode to "circularizing ( " + Round(NextNode:DeltaV:MAG) + " m/s )". Print Mode + Spacer(Mode:Length, 36) at (11, 5). } Else { Set Mode to "coast to apoapsis " + FormatTime(ABS((Time:Seconds - StartTime))). Print Mode + Spacer(Mode:Length, 36) at (11, 5). }
    If Time:Seconds >= StartTime - 10 { If Warp > 0 Set Warp to 0. }
    If Time:Seconds >= StartTime { If Ens_Thrust > 0 and Ens_Isp > 0 { If Warp > 0 Set Warp to 0.
      Lock Throttle to Max(Min(((Body:Mu / Body:Radius^2) * (Mass * 1000) * (Ens_Isp / Ens:Length) * (1 - Constant:E^(-NextNode:BurnVector:MAG/((Body:Mu / Body:Radius^2) * (Ens_Isp / Ens:Length)))) / (Ens_Thrust * 1000)), 1), 0.05).
      When VDot(NextNode:BurnVector, BV) < 0 or NextNode:DeltaV:MAG < 0.1 Then { Lock Throttle to 0. Unlock Steering. Set RunExecNode to "Completed". Wait 0. }
    } Else { Lock Throttle to 0. Unlock Steering. Wait 0. Set RunExecNode to "Failed". }} Preserve.
  }
}

// .=========.
// | VARIOUS |
// |=========|

Function Spacer { Parameter StringLength, SpaceLength. Local Spaces is "                                                  ":Substring(0, Max(Min(SpaceLength, 50), 0)). If StringLength >= SpaceLength Return "". Return Spaces:Substring(StringLength, Spaces:Length - StringLength). }

Function NoEngines {
  Print "                 LAUNCH AUTOPILOT                " at (0, 2).
  Print "      .=====================================.    " at (0, 10).
  Print "      |             NO ENGINES              |    " at (0, 11).
  Print "      |=====================================|    " at (0, 12).
  Print "  By: Kobayashi " at (30, 30).
  Until False { If Terminal:Input:HasChar { If Terminal:Input:GetChar() = Terminal:Input:Enter Reboot. }}
}

Function InOrbit {
  Print "                 LAUNCH AUTOPILOT                " at (0, 2).
  Print "      .=====================================.    " at (0, 10).
  Print "      |              IN ORBIT               |    " at (0, 11).
  Print "      |=====================================|    " at (0, 12).
  Print "  By: Kobayashi " at (30, 30).
  Until False { If Terminal:Input:HasChar { If Terminal:Input:GetChar() = Terminal:Input:Enter Reboot. }}
}

Function InDeOrbit {
  Print "                 LAUNCH AUTOPILOT                " at (0, 2).
  Print "      .=====================================.    " at (0, 10).
  Print "      |              REENTRING              |    " at (0, 11).
  Print "      |=====================================|    " at (0, 12).
  Print "  By: Kobayashi " at (30, 30).
  Until False { If Terminal:Input:HasChar { If Terminal:Input:GetChar() = Terminal:Input:Enter Reboot. }}
}

Function TargetExist { Parameter cName. If cName = "None" Return False. List Targets in TargetList. For Tar in TargetList { If Tar:Body = Ship:Body and Tar:Type <> "Debris" and Tar:Status = "Orbiting" and Tar:Name = cName Return True. } Return False. }
Function GetTargetList { Set MyTargets to List("None"). List Targets in TargetList. For Tar in TargetList { If Tar:Body = Ship:Body and Tar:Type <> "Debris" and Tar:Status = "Orbiting" MyTargets:Add(Tar:Name). } Return MyTargets. }