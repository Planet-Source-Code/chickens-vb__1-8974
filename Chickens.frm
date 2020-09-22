VERSION 5.00
Begin VB.Form Chickens 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Chickens VB"
   ClientHeight    =   5805
   ClientLeft      =   45
   ClientTop       =   615
   ClientWidth     =   7575
   Icon            =   "Chickens.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5805
   ScaleWidth      =   7575
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer3 
      Left            =   840
      Top             =   0
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Select Gattling Gun"
      Enabled         =   0   'False
      Height          =   375
      Left            =   5160
      TabIndex        =   4
      Top             =   840
      Width           =   2295
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Select Shotgun"
      Enabled         =   0   'False
      Height          =   375
      Left            =   2640
      TabIndex        =   3
      Top             =   840
      Width           =   2295
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Select Riffle"
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   840
      Width           =   2295
   End
   Begin VB.Timer Timer2 
      Interval        =   50
      Left            =   7080
      Top             =   0
   End
   Begin VB.Timer Timer1 
      Interval        =   10
      Left            =   120
      Top             =   0
   End
   Begin VB.PictureBox Screen 
      Height          =   4095
      Left            =   0
      MousePointer    =   2  'Cross
      ScaleHeight     =   4035
      ScaleWidth      =   7515
      TabIndex        =   0
      Top             =   1320
      Width           =   7575
      Begin VB.Image Chicken 
         Enabled         =   0   'False
         Height          =   345
         Index           =   7
         Left            =   6360
         Picture         =   "Chickens.frx":030A
         Tag             =   "-4"
         Top             =   1440
         Width           =   330
      End
      Begin VB.Image Chicken 
         Enabled         =   0   'False
         Height          =   345
         Index           =   6
         Left            =   6405
         Picture         =   "Chickens.frx":06BB
         Tag             =   "-4"
         Top             =   1920
         Width           =   330
      End
      Begin VB.Image Chicken 
         Enabled         =   0   'False
         Height          =   345
         Index           =   5
         Left            =   1440
         Picture         =   "Chickens.frx":0A6C
         Tag             =   "4"
         Top             =   2040
         Width           =   330
      End
      Begin VB.Image Chicken 
         Enabled         =   0   'False
         Height          =   345
         Index           =   4
         Left            =   1440
         Picture         =   "Chickens.frx":0E1E
         Tag             =   "4"
         Top             =   1560
         Width           =   330
      End
      Begin VB.Image Billy 
         Enabled         =   0   'False
         Height          =   345
         Index           =   4
         Left            =   720
         Picture         =   "Chickens.frx":11D0
         Tag             =   "4"
         Top             =   1560
         Visible         =   0   'False
         Width           =   330
      End
      Begin VB.Shape GunSight 
         Height          =   375
         Left            =   1440
         Shape           =   3  'Circle
         Top             =   480
         Width           =   375
      End
      Begin VB.Image Chicken 
         Enabled         =   0   'False
         Height          =   345
         Index           =   3
         Left            =   3500
         Tag             =   "4"
         Top             =   2040
         Width           =   330
      End
      Begin VB.Image Chicken 
         Enabled         =   0   'False
         Height          =   345
         Index           =   2
         Left            =   800
         Picture         =   "Chickens.frx":1582
         Tag             =   "4"
         Top             =   2040
         Width           =   330
      End
      Begin VB.Image Chicken 
         Enabled         =   0   'False
         Height          =   345
         Index           =   1
         Left            =   5760
         Picture         =   "Chickens.frx":1934
         Tag             =   "-4"
         Top             =   1920
         Width           =   330
      End
      Begin VB.Image Chicken 
         Enabled         =   0   'False
         Height          =   345
         Index           =   0
         Left            =   5760
         Picture         =   "Chickens.frx":1CE5
         Tag             =   "-4"
         Top             =   1440
         Width           =   330
      End
   End
   Begin VB.Shape GotGun3 
      BorderWidth     =   5
      Height          =   375
      Left            =   5160
      Top             =   840
      Visible         =   0   'False
      Width           =   2295
   End
   Begin VB.Shape GotGun2 
      BorderWidth     =   5
      Height          =   375
      Left            =   2640
      Top             =   840
      Visible         =   0   'False
      Width           =   2295
   End
   Begin VB.Shape GotGun1 
      BorderWidth     =   5
      Height          =   375
      Left            =   120
      Top             =   840
      Width           =   2295
   End
   Begin VB.Shape AmmoBAr 
      BorderColor     =   &H80000006&
      FillColor       =   &H000000FF&
      FillStyle       =   0  'Solid
      Height          =   255
      Left            =   120
      Top             =   5520
      Width           =   7335
   End
   Begin VB.Label DisplayScore 
      Caption         =   "Score - 0"
      BeginProperty Font 
         Name            =   "Comic Sans MS"
         Size            =   21.75
         Charset         =   161
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   1920
      TabIndex        =   1
      Top             =   0
      Width           =   4575
   End
   Begin VB.Menu M1 
      Caption         =   "Options"
      Begin VB.Menu OPT1 
         Caption         =   "Settings"
         Shortcut        =   {F1}
      End
      Begin VB.Menu Ins 
         Caption         =   "Instructions"
      End
   End
End
Attribute VB_Name = "Chickens"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim X As Integer
Dim Blood(500, 10)
Dim hights(10000) As Integer
Dim LAND
Dim ChickCount As Integer
Dim Sky(3) As Integer
Dim Xmouse As Integer
Dim Ymouse As Integer
Dim chicklist As Integer
Dim score As Long
Dim GunType As Integer
Dim Rapid As Boolean
Dim TotolBlood As Integer
Dim BOSS(2, 10)
Dim bloodmode As Integer
Dim Bxx, Byy As Integer

Private Sub Command1_Click()
GunType = 1
GotGun1.Visible = True
GotGun2.Visible = False
GotGun3.Visible = False
GunSight.Width = 375
GunSight.Height = 375
End Sub

Private Sub Command2_Click()
GunType = 2
GotGun1.Visible = False
GotGun2.Visible = True
GotGun3.Visible = False
GunSight.Width = 800
GunSight.Height = 800
End Sub

Private Sub Command3_Click()
GunType = 3
GotGun1.Visible = False
GotGun2.Visible = False
GotGun3.Visible = True
GunSight.Width = 375
GunSight.Height = 375
End Sub

Private Sub Command4_Click()

End Sub

Private Sub Form_Load()
TotolBlood = 500
On Error GoTo FoundAll
Do:
Chicken(ChickCount).Visible = False
ChickCount = ChickCount + 1
Loop

For n = 0 To ChickCount - 1
Chicken(n).Visible = True
Chicken(n).Left = -100

Next n

Chicken(0).Tag = 0
Chicken(1).Tag = 0
Chicken(2).Tag = 0
Chicken(3).Tag = 0


FoundAll:

Chickens.Visible = True
Call SetUpnewGame
Chickens.Enabled = False
Options.Visible = True
Chickens.Enabled = False
Timer1.Interval = 0
End Sub

Sub MakeNew()
Screen.DrawWidth = 1
score = 0
AmmoBAr.Width = 7335
Chickens.Refresh
 For chicklist = 0 To ChickCount - 1
  Chicken(chicklist).Visible = False
 Next chicklist
 
For n = 1 To TotolBlood
 Blood(n, 10) = 0
Next n

If Rnd > 0.3 Then Sky(1) = 100: Sky(2) = 100: Sky(3) = 200
If Rnd > 0.4 Then Sky(1) = 200: Sky(2) = 150: Sky(3) = 150
If Rnd > 0.7 Then Sky(1) = 150: Sky(2) = 150: Sky(3) = 150

Screen.AutoRedraw = True

For n = 1 To Screen.Height Step 15
 col = (255 / Screen.Height) * n
 Screen.Line (Screen.Width, n)-(-17, n), RGB(col, Sky(2), Sky(3))
Next n

Skale = Options.LandRes.Value

Screen.DrawWidth = Skale
Dim marker As Integer
Dim DIR
Randomize Timer
LAND = Screen.Height / 2
Dim Drk
ArrayLocation = 0
For n = 1 To Screen.Width Step 15
ArrayLocation = ArrayLocation + 1
If Rnd > 0.9 Then
X = Int(Rnd * 5)
If X = 0 Then DIR = -0.5 * Options.Hills.Value
If X = 1 Then DIR = -0.25 * Options.Hills.Value
If X = 2 Then DIR = 0 * Options.Hills.Value
If X = 3 Then DIR = 0.25 * Options.Hills.Value
If X = 4 Then DIR = 0.5 * Options.Hills.Value
If X = 5 Then DIR = -0.25 * Options.Hills.Value
End If
If LAND > (Screen.Height / 4) * 2 Then DIR = -10
If LAND < (Screen.Height / 6) Then DIR = 10
LAND = LAND + DIR

hights(ArrayLocation - 1) = LAND

marker = marker + 1
If marker = Skale Then
 marker = 0
'Screen.Line (n, Screen.Height)-(n, Screen.Height - LAND), RGB(0, (Rnd * 100) + 100, 0)
 For m% = Screen.Height To Screen.Height - LAND Step -15 * Skale
 Screen.PSet (n, m), RGB(0, (Rnd * 100) + 100, 0)
 Next m%
Screen.Refresh
 End If
Next n

 For chicklist = 0 To ChickCount - 1
  Chicken(chicklist).Visible = True
 Next chicklist

Timer1.Interval = 0
Options.Visible = True
End Sub


Private Sub Ins_Click()
writ$ = writ$ + "Stop the chicken running around the screen!!" + Chr$(13)
writ$ = writ$ + "Click the mouse on them, to blow them to shite," + Chr$(13)
writ$ = writ$ + "and attempt to collect the white crystal things that" + Chr$(13)
writ$ = writ$ + "come out of some of them, to increase your ammo" + Chr$(13)
writ$ = writ$ + "" + Chr$(13)
writ$ = writ$ + "The more points you get, the more weapons become avaliable-" + Chr$(13)
writ$ = writ$ + " Riffle. Slow fire rate, with a small area of damage" + Chr$(13)
writ$ = writ$ + " Shotgun. Larger damage area, to kill more chickens" + Chr$(13)
writ$ = writ$ + " Gattling gun. Small damage area, but rapid fire" + Chr$(13)
writ$ = writ$ + "" + Chr$(13)


MsgBox writ$, vbOKOnly, "Instructions"


End Sub

Private Sub OPT1_Click()
Options.Visible = True
Timer1.Interval = 0
Chickens.Enabled = False
Options.SetFocus
End Sub

Private Sub Screen_Click()
Call Fire
End Sub


Private Sub Screen_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)

GunSight.Left = X - (GunSight.Width / 2)
GunSight.Top = Y - (GunSight.Width / 2)
Xmouse = X: Ymouse = Y
If GunType = 3 And Button = 1 Then
   Call Fire
  End If
End Sub

Private Sub Timer1_Timer()

If Billy(4).Visible = True Then
 Billy(4).Left = Billy(4).Left + BOSS(1, 1) * 5
 If Billy(4).Left < 0 Then BOSS(1, 1) = 50: Billy(4).Visible = False
 If Billy(4).Left > Screen.Width - Billy(4).Width Then BOSS(1, 1) = -50: AmmoBAr.Width = AmmoBAr.Width - 600
End If

 If score >= 400 Then Command2.Enabled = True
 If score >= 600 Then Command3.Enabled = True
 If score >= 800 Then Timer3.Interval = 8000
 
 If Options.FlashShot.Value <> False Then Chickens.BackColor = &H8000000F
 For n = 0 To ChickCount - 1
  Chicken(n).Left = Chicken(n).Left + (Chicken(n).Tag * 17)
 
  If Chicken(n).Left < 0 And Chicken(n).Tag = -4 Then
    If Options.Mode.Value <> False Then Call ChicWinner: Exit Sub
    Chicken(n).Left = Screen.Width - Chicken(n).Width + (Rnd * 10000)
  End If
  If Chicken(n).Left > Screen.Width And Chicken(n).Tag = 4 Then
    If Options.Mode.Value <> False Then Call ChicWinner: Exit Sub
    Chicken(n).Left = 1 - (Rnd * 10000)
  End If
  If Chicken(n).Left < 1 Then GoTo ZeroOrMassive:
  Chicken(n).Top = (Screen.Height - hights(Int((Chicken(n).Left) / 15)) - 50) - 140
 
ZeroOrMassive:
 Next n
End Sub

Sub Bleed()
Counta = 0
allCry = Int(Rnd * 1)
For n = 1 To TotolBlood
 If Blood(n, 10) = 0 Then
  Blood(n, 2) = Byy + 100
  Blood(n, 1) = Bxx
  Blood(n, 4) = Byy + 100
  Blood(n, 3) = Bxx
  
  dir1 = Xmouse - Bxx
  dir2 = Ymouse - Byy
  
  Blood(n, 5) = ((300 - dir1) / 10) + ((Rnd * 30) - 15)
  Blood(n, 6) = (Rnd * 50) - 150
  
  
  Blood(n, 10) = 1
  If Rnd > 0.8 Then Blood(n, 10) = 3
  If Rnd > 0.95 Then
    Blood(n, 10) = 2
    Blood(n, 6) = -80 - Rnd * 20
    Blood(n, 2) = Byy - 500
    Blood(n, 4) = Byy - 500
  End If
   If bloodmode = 1 Then
     Blood(n, 10) = 4
     If allCry = 1 Then Blood(n, 10) = 2
     Blood(n, 5) = (Rnd * 160) - 80
     Blood(n, 6) = (Rnd * 160) - 120
    Counta = Counta - 0.8
   End If

  Counta = Counta + 1
  If Counta >= Options.Gore Then Exit Sub
 End If
Next n
End Sub

Private Sub Timer2_Timer()
For n% = 1 To TotolBlood
  If Blood(n%, 10) <> 0 Then
    Screen.DrawWidth = 5
    col = (255 / Screen.Height) * Blood(n%, 4)
    If col < 0 Then col = 0
    Screen.PSet (Blood(n%, 3), Blood(n%, 4)), RGB(col, Sky(2), Sky(3))
    Blood(n%, 1) = Blood(n%, 1) + Blood(n%, 5)
    Blood(n%, 2) = Blood(n%, 2) + Blood(n%, 6)
    Blood(n%, 6) = Blood(n%, 6) + 10
    If (Blood(n%, 1) / 15) - 2 < 0 Then Blood(n%, 10) = 0: GoTo Here
    
    If Blood(n, 10) = 1 Then Screen.PSet (Blood(n%, 1), Blood(n%, 2)), RGB(150 + Rnd * 100, 0, 0)
    If Blood(n, 10) = 3 Then Screen.PSet (Blood(n%, 1), Blood(n%, 2)), RGB(255, 255, 0)
    If Blood(n, 10) = 4 Then Screen.PSet (Blood(n%, 1), Blood(n%, 2)), RGB(0, 200 + Rnd * 50, 0)
    
    If Blood(n, 10) = 2 Then
      Blood(n%, 6) = Blood(n%, 6) - 7
      Screen.PSet (Blood(n%, 1), Blood(n%, 2)), RGB(255, 255, 255)
      dirup = Blood(n%, 2) - Ymouse
      dirleft = Blood(n%, 1) - Xmouse
      GunToCrystal = Int(Sqr(dirup ^ 2 + dirleft ^ 2))
      If GunToCrystal < 200 Then
         Blood(n%, 10) = 0
         AmmoBAr.Width = AmmoBAr.Width + 400
         If AmmoBAr.Width > 7335 Then AmmoBAr.Width = 7335
         Screen.PSet (Blood(n%, 1), Blood(n%, 2)), RGB(col, Sky(2), Sky(3))
         If Options.FlashShot.Value <> False Then Chickens.BackColor = &HFFFFFF

      End If
    End If
    Blood(n%, 3) = Blood(n%, 1)
    Blood(n%, 4) = Blood(n%, 2)
    If (Blood(n%, 1) / 15) - 2 < 0 Then Blood(n%, 10) = 0: GoTo Here
    If Blood(n%, 1) > Screen.Width - 3 Then Blood(n%, 10) = 0: GoTo Here
    
    If Screen.Height - Blood(n%, 2) < hights(Blood(n, 1) / 15) Then
       Blood(n, 10) = 0
       If Screen.Height - Blood(n%, 2) + 100 > hights(Blood(n, 1) / 15) Then
       hights(Blood(n, 1) / 15) = hights(Blood(n, 1) / 15) + 10
       hights((Blood(n, 1) / 15) - 1) = hights((Blood(n, 1) / 15) - 1) + 5
       hights((Blood(n, 1) / 15) + 1) = hights((Blood(n, 1) / 15) + 1) + 5
       hights((Blood(n, 1) / 15) - 2) = hights((Blood(n, 1) / 15) - 2) + 5
       hights((Blood(n, 1) / 15) + 2) = hights((Blood(n, 1) / 15) + 2) + 5
      End If
    End If
Here:
  End If
Next n%
End Sub

Sub Fire()
 Spread = 300
 If GunType = 2 Then Spread = 600
 On Error GoTo fag
  
 AmmoBAr.Width = AmmoBAr.Width - 150
 If GunType = 3 Then AmmoBAr.Width = AmmoBAr.Width + 100
GoTo notthis
fag:
  Call died
notthis:



  dirup = (Billy(4).Top + (Billy(4).Height / 2)) - Ymouse
  dirleft = (Billy(4).Left + (Billy(4).Width / 2)) - Xmouse
  GunToBill = Int(Sqr(dirup ^ 2 + dirleft ^ 2))
   If GunToBill < Spread Then
      Billy(4).Visible = False
      bloodmode = 1
      score = score + 50
          Bxx = Billy(4).Left + Billy(4).Width
          Byy = Billy(4).Top + (Billy(4).Height / 2)
      Billy(4).Left = -1200
      Call Bleed
   End If

      bloodmode = 0

 If Options.FlashShot.Value <> False Then Chickens.BackColor = 255
 For chicklist = 0 To ChickCount - 1
  dirup = (Chicken(chicklist).Top) - Ymouse
  dirleft = (Chicken(chicklist).Left) - Xmouse
  GunToChick = Int(Sqr(dirup ^ 2 + dirleft ^ 2))
  'MsgBox GunToChick
  If GunToChick < Spread Then
  'Bang, got the bastard...
    Bxx = Chicken(chicklist).Left
    Byy = Chicken(chicklist).Top
     Call Bleed
     
   If Chicken(chicklist).Tag = -4 Then
    Chicken(chicklist).Left = Screen.Width - Chicken(chicklist).Width + (Rnd * 10000)
  End If
  If Chicken(chicklist).Tag = 4 Then
    Chicken(chicklist).Left = 1 - (Rnd * 10000)
  End If

     score = score + 10
  DisplayScore.Caption = "Score - " + Str$(score)
  
  
  End If
 Next chicklist
End Sub



Sub died()
responce = MsgBox("You ran out of ammo, you total loser!" + Chr$(13) + "Do you want to play again?", vbYesNo, "Chickens VB")
If responce = 7 Then End
Call SetUpnewGame
End Sub

Private Sub Timer3_Timer()
If score < 400 Then Exit Sub
If Billy(4).Visible = True Then Exit Sub
If Rnd > 0.5 Then Exit Sub
Billy(4).Visible = True
Billy(4).Left = 0
BOSS(1, 1) = 50
End Sub


Sub ChicWinner()

responce = MsgBox("You let a chicken get across the screen, you total loser!" + Chr$(13) + "Do you want to play again?", vbYesNo, "Chickens VB")
If responce = 7 Then End
Call SetUpnewGame
End Sub


Sub SetUpnewGame()
 For chicklist = 0 To ChickCount - 1
  Chicken(chicklist).Visible = False
  If Chicken(chicklist).Tag = 4 Then Chicken(chicklist).Left = -(1000 + Rnd * 4000)
  If Chicken(chicklist).Tag = -4 Then Chicken(chicklist).Left = Screen.Width + (1000 + Rnd * 4000)
   
 Next chicklist
For n = 1 To TotolBlood
 Blood(n, 10) = 0
Next n
Screen.Cls
AmmoBAr.Width = 7335
score = 0
Command2.Enabled = False
Command3.Enabled = False
GunType = 1
GotGun1.Visible = True
GotGun2.Visible = False
GotGun3.Visible = False
GunSight.Width = 375
GunSight.Height = 375
Billy(4).Left = -2000
Billy(4).Visible = False
Timer3.Interval = 0
Timer1.Interval = 0
DisplayScore.Caption = "Score - " + Str$(score)
Call MakeNew
Chickens.Enabled = False
Options.Visible = True

End Sub

