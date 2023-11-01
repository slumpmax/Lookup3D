unit XPWindow;

interface

uses
  XPGL, XPTexture,
  Forms, Windows, Controls, SysUtils, dglOpenGL;

type
  TXPWindow = class
  private
    FTexture: TXPTexture;
  public
    X, Y, Width, Height: Integer;
    Alpha, ZOrder: Single;
    Visible: Boolean;
    constructor Create(ATexture: TXPTexture);
    procedure Render;
    procedure DrawMouse(APos: TPoint);
    procedure SetSize(AWidth, AHeight: Integer);
  end;

implementation

constructor TXPWindow.Create(ATexture: TXPTexture);
begin
  FTexture := ATexture;
  X := 0;
  Y := 0;
  ZOrder := 0.0;
  Width := 400;
  Height := 300;
  Alpha := 1.0;
  Visible := True;
end;

procedure TXPWindow.Render;
begin
  if Visible then
  begin
    FTexture.GL.Activate;
    FTexture.GL.SetOrtho;
    glBindTexture(GL_TEXTURE_2D, FTexture.Handle);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_TEXTURE_2D);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glColor4f(1.0, 1.0, 1.0, Alpha);

    glPushMatrix();
    glTranslatef(X, FTexture.GL.Height - Y, ZOrder);
    glBegin(GL_QUADS);
    // bottom left corner of window.
    glTexCoord2f( 0/128, 1);        glVertex2f(0, -Height);
    glTexCoord2f( 0/128, 1-27/128); glVertex2f(0, 27 - Height);
    glTexCoord2f(64/128, 1-27/128); glVertex2f(63, 27 - Height);
    glTexCoord2f(64/128, 1);        glVertex2f(63, -Height);

    // bottom of window.
    glTexCoord2f(64/128, 1);        glVertex2f(63, -Height);
    glTexCoord2f(64/128, 1-27/128); glVertex2f(63, 27 - Height);
    glTexCoord2f(96/128, 1-27/128); glVertex2f(Width-32, 27 - Height);
    glTexCoord2f(96/128, 1);        glVertex2f(Width-32, -Height);

    // bottom right corder of window.
    glTexCoord2f(96/128, 1);        glVertex2f(Width-32, -Height);
    glTexCoord2f(96/128, 1-27/128); glVertex2f(Width-32, 27 - Height);
    glTexCoord2f(1, 1-27/128);      glVertex2f(Width, 27 - Height);
    glTexCoord2f(1, 1);             glVertex2f(Width, -Height);

    // left side of window.
    glTexCoord2f(0/128, 1-27/128);  glVertex2f(0, 27 - Height);
    glTexCoord2f(0/128, 27/128);    glVertex2f(0, -27);
    glTexCoord2f(6/128, 27/128);    glVertex2f(6, -27);
    glTexCoord2f(6/128, 1-27/128);  glVertex2f(6, 27 - Height);

    // draw the main body of the window
    glTexCoord2f( 8/128, 63/128);     glVertex2f(6, 27-Height);
    glTexCoord2f( 8/128, 32/128);     glVertex2f(6, -27);
    glTexCoord2f(39/128, 32/128);     glVertex2f(Width-7, -27);
    glTexCoord2f(39/128, 63/128);     glVertex2f(Width-7, 27-Height);

    // right side of window.
    glTexCoord2f(1-7/128, 1-27/128);glVertex2f(Width-7, 27 - Height);
    glTexCoord2f(1-7/128, 27/128);  glVertex2f(Width-7, -27);
    glTexCoord2f(1,   27/128);      glVertex2f(Width, -27);
    glTexCoord2f(1, 1-27/128);      glVertex2f(Width, 27 - Height);

    // top left corner of window.
    glTexCoord2f( 0/128, 27/128);   glVertex2f(0, -27);
    glTexCoord2f( 0/128,  0/128);   glVertex2f(0, 0);
    glTexCoord2f(64/128,  0/128);   glVertex2f(63, 0);
    glTexCoord2f(64/128, 27/128);   glVertex2f(63, -27);

    // top of window.
    glTexCoord2f(64/128, 27/128);   glVertex2f(63, -27);
    glTexCoord2f(64/128, 0/128);    glVertex2f(63, 0);
    glTexCoord2f(96/128, 0/128);    glVertex2f(Width-32, 0);
    glTexCoord2f(96/128, 27/128);   glVertex2f(Width-32, -27);

    // top right corner of window.
    glTexCoord2f(96/128, 27/128);   glVertex2f(Width-32, -27);
    glTexCoord2f(96/128, 0/128);    glVertex2f(Width-32, 0);
    glTexCoord2f(1, 0/128);         glVertex2f(Width, 0);
    glTexCoord2f(1, 27/128);        glVertex2f(Width, -27);

    // window close button
    glTexCoord2f(104/128, 32/128); glVertex3f(Width-22, -8, 0.01);
    glTexCoord2f(104/128, 48/128); glVertex3f(Width-22, -24, 0.01);
    glTexCoord2f(120/128, 48/128); glVertex3f(Width-6, -24, 0.01);
    glTexCoord2f(120/128, 32/128); glVertex3f(Width-6, -8, 0.01);
    glEnd();

    glPopMatrix();
  end;
end;

procedure TXPWindow.SetSize(AWidth, AHeight: Integer);
begin
  Width := AWidth;
  Height := AHeight;
end;

Procedure TXPWindow.DrawMouse(APos: TPoint);
begin
  FTexture.GL.Activate;
  FTexture.GL.SetOrtho;
  glBindTexture(GL_TEXTURE_2D, FTexture.Handle);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glColor4f(1.0, 1.0, 1.0, Alpha);
  glPushMatrix();
  glTranslatef(APos.X, FTexture.GL.Height - APos.Y, ZOrder);
  glBegin(GL_QUADS);
    glTexCoord2f(40/128, 64/128); glVertex2i(0, 0);
    glTexCoord2f(72/128, 64/128); glVertex2i(32, 0);
    glTexCoord2f(72/128, 96/128); glVertex2i(32, -32);
    glTexCoord2f(40/128, 96/128); glVertex2i(0, -32);
  glEnd;
  glPopMatrix();
end;

end.
