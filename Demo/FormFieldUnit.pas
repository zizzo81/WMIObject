unit FormFieldUnit;

{******************************************************************************}
{                                                                              }
{     WMI query simple class for Delphi Demo application                       }
{     Version 1.0 released October, 29th 2023                                  }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ MIT License                                                                  }
{                                                                              }
{ Copyright (c) 2023, Christian Cristofori <github@christiancristofori.it>     }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to     }
{ deal in the Software without restriction, including without limitation the   }
{ rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  }
{ sell copies of the Software, and to permit persons to whom the Software is   }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      }
{ FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS }
{ IN THE SOFTWARE.                                                             }
{                                                                              }
{ If you have this source, you can use it as you want, how the hell am I going }
{ to stop you from using it anyway? Being mentioned in your apps'documentation }
{ and/or about box will be greatly appreciated, although not necessary.        }
{ Feedback is appreciated too.                                                 }
{                                                                              }
{ You can get in touch with me at any time.                                    }
{                                                                              }
{******************************************************************************}

interface

uses
  Forms, StdCtrls, Controls, Classes;

type
  TFormField = class(TForm)
    lField: TLabel;
    lIndex: TLabel;
    eField: TEdit;
    eIndex: TEdit;
    eValue: TEdit;
    lValue: TLabel;
    bClose: TButton;
    lType: TLabel;
    eType: TEdit;
  end;

implementation

{$R *.dfm}

end.
