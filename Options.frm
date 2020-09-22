VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Options 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Options"
   ClientHeight    =   4185
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   3585
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   Moveable        =   0   'False
   ScaleHeight     =   4185
   ScaleWidth      =   3585
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Begin MSComctlLib.Slider LandRes 
      Height          =   615
      Left            =   1440
      TabIndex        =   10
      Top             =   2880
      Width           =   1935
      _ExtentX        =   3413
      _ExtentY        =   1085
      _Version        =   393216
      LargeChange     =   1
      Min             =   1
      SelStart        =   1
      TickStyle       =   2
      Value           =   1
   End
   Begin VB.CheckBox Mode 
      Caption         =   "You die when a chicken makes it across the screen (Hard Mode)"
      Height          =   375
      Left            =   120
      TabIndex        =   9
      Top             =   2400
      Width           =   3015
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Draw New Level"
      Height          =   375
      Left            =   1800
      TabIndex        =   4
      Top             =   3600
      Width           =   1575
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Play Game"
      Height          =   375
      Left            =   240
      TabIndex        =   3
      Top             =   3600
      Width           =   1575
   End
   Begin VB.CheckBox FlashShot 
      Caption         =   "Flash background when you fire or collect a crystal"
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   1920
      Width           =   3135
   End
   Begin VB.HScrollBar Hills 
      Height          =   255
      Left            =   360
      Max             =   50
      TabIndex        =   1
      Top             =   600
      Value           =   15
      Width           =   2775
   End
   Begin VB.HScrollBar Gore 
      Height          =   255
      Left            =   360
      Max             =   50
      Min             =   1
      TabIndex        =   0
      Top             =   1440
      Value           =   5
      Width           =   2775
   End
   Begin VB.Label Label5 
      Caption         =   "Detail of the land"
      Height          =   255
      Left            =   120
      TabIndex        =   11
      Top             =   3120
      Width           =   1215
   End
   Begin VB.Label Label4 
      Caption         =   "Blood-bath"
      Height          =   255
      Left            =   2400
      TabIndex        =   8
      Top             =   1080
      Width           =   1215
   End
   Begin VB.Label Label3 
      Caption         =   "Kids only"
      Height          =   255
      Left            =   120
      TabIndex        =   7
      Top             =   1080
      Width           =   1215
   End
   Begin VB.Label Label2 
      Caption         =   "Mountins"
      Height          =   255
      Left            =   2640
      TabIndex        =   6
      Top             =   240
      Width           =   735
   End
   Begin VB.Label Label1 
      Caption         =   "Flat"
      Height          =   255
      Left            =   240
      TabIndex        =   5
      Top             =   240
      Width           =   855
   End
End
Attribute VB_Name = "Options"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
Options.Visible = False
Chickens.Enabled = True
Chickens.SetFocus
Chickens.Timer1.Interval = 10
End Sub

Private Sub Command2_Click()
responce = MsgBox("Are you sure you want to start a new game?", vbYesNo, "Chickens VB")
If responce = 7 Then Exit Sub
Options.Visible = False
Call Chickens.SetUpnewGame
End Sub

Private Sub Form_GotFocus()
Chickens.Timer1.Interval = 0
Chickens.Enabled = False
End Sub

Private Sub Form_Load()
Chickens.Timer1.Interval = 0
Chickens.Enabled = False
End Sub
