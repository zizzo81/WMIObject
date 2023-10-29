program WMIObjectDemo;

{***************************************************************************}
{                                                                           }
{     WMI query simple class for Delphi Demo application                    }
{     Version 1.0.0.0 released July, 22nd 2022                              }
{                                                                           }
{     Copyright (C) 2016 Christian Cristofori                               }
{                        github@christiancristofori.it                      }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the GNU Lesser General Public License, Version 3;         }
{  you may not use this file except in compliance with the License.         }
{                                                                           }
{  This is free software: you can redistribute it and/or modify it under    }
{  the terms of the GNU Lesser General Public License as published by the   }
{  Free Software Foundation, either version 3 of the License, or (at your   }
{  option) any later version.                                               }
{                                                                           }
{  This is distributed in the hope that it will be useful, but WITHOUT      }
{  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or    }
{  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public      }
{  License for more details.                                                }
{                                                                           }
{  You should have received a copy of the GNU Lesser General Public         }
{  License along with this software.                                        }
{  If not, see <http://www.gnu.org/licenses/>.                              }
{                                                                           }
{  If you have this source, you can use it as you want, how the hell am I   }
{  going to stop you from using it anyway? Being mentioned in your          }
{  applications' documentation or about box will be greatly appreciated,    }
{  although not necessary. Feedback is appreciated too.                     }
{                                                                           }
{  You can get in touch with me at any time.                                }
{                                                                           }
{***************************************************************************}

uses
  Forms,
  WbemScripting_TLB in '..\Source\WbemScripting_TLB.pas',
  WMIObject in '..\Source\WMIObject.pas',
  FormMainUnit in 'FormMainUnit.pas' {FormMain},
  FormArrayUnit in 'FormArrayUnit.pas' {FormArray},
  FormFieldUnit in 'FormFieldUnit.pas' {FormField};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'WMIObject Demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
