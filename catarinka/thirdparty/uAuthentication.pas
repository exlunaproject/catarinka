unit uAuthentication;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons;

type
  TPasswordDlg = class(TForm)
    Label1: TLabel;
    edtPassword: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    Label2: TLabel;
    edtusername: TEdit;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  PasswordDlg: TPasswordDlg;

implementation

{$R *.dfm}

end.
 
