unit TestUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MyListViewUnit, Vcl.ComCtrls,
  Vcl.ImgList, MyToolbarUnit, Vcl.ToolWin, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    MyToolBar1: TMyToolBar;
    ToolButton11: TMyToolButton;
    ToolButton12: TMyToolButton;
    ToolButton13: TMyToolButton;
    ToolButton14: TMyToolButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FList: TMyListView;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  col: TMyListColumn;
  i: Integer;
begin
  FList := TMyListView.Create(Self);
  FList.Align := alClient;
  FList.ViewStyle := vsReport;
  FList.HeaderImages := ImageList1;
  FList.Parent := Self;
  col := FList.Columns.Add;
  col.Width := 100;
  col.Caption := 'Column 1';
  col.SortOrder := soASC;
  col := FList.Columns.Add;
  col.Caption := 'Column 2';
  col.Width := 100;
  col.AutoSize := True;
//  FList.UpdateColumns;
  for i := 0 to 100 do
    FList.Items.Add.Caption := Format('Item %d', [i]);
//  Application.ProcessMessages;
  FList.Items.BeginUpdate;
  try
    FList.FixColumnsWidth;
    FList.ResortItems(nil);
    FList.UpdateHeaderState;
  finally
    FList.Items.EndUpdate;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
//  FList.UpdateHeaderState;
end;

end.
