unit visitors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject, tiVisitorDB, baseopf, mdltodo,
  mdlproject, mdlperson, mdlInterrupt, mdltask, mdltimetrack;

Type
  { TReadProjectVisitor }
  TReadProjectVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TReadListProjectVisitor }
  TReadListProjectVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TSaveProjectVisitor }
  TSaveProjectVisitor = Class(TtiVisitorUpdate)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
  end;

  { TReadPersonVisitor }
  TReadPersonVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TReadListPersonVisitor }
  TReadListPersonVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TSavePersonVisitor }
  TSavePersonVisitor = Class(TtiVisitorUpdate)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
  end;

  { TReadTodoVisitor }
  TReadTodoVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TReadListTodoVisitor }
  TReadListTodoVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TSaveTodoVisitor }
  TSaveTodoVisitor = Class(TtiVisitorUpdate)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
  end;

  { TReadTaskVisitor }
  TReadTaskVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TReadListTaskVisitor }
  TReadListTaskVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TSaveTaskVisitor }
  TSaveTaskVisitor = Class(TtiVisitorUpdate)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
  end;


  { TReadInterruptVisitor }
  TReadInterruptVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TReadListInterruptVisitor }
  TReadListInterruptVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TSaveInterruptVisitor }
  TSaveInterruptVisitor = Class(TtiVisitorUpdate)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
  end;

  { TReadTimeTrackVisitor }
  TReadTimeTrackVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TReadListTimeTrackVisitor }
  TReadListTimeTrackVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TReadTaskTimeTrackVisitor }
  TReadTaskTimeTrackVisitor = Class(TtiVisitorSelect)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
    Procedure MapRowToObject; override;
  end;

  { TSaveTimeTrackVisitor }
  TSaveTimeTrackVisitor = Class(TtiVisitorUpdate)
  Protected
    Procedure Init; override;
    Function AcceptVisitor : Boolean; override;
    Procedure SetupParams; override;
  end;

Var
  LoadPersonsList  : TPersonList;
  LoadProjectsList : TProjectList;
  LoadTasksList : TTaskList;

implementation

Const
  SQLReadProject = 'SELECT PJ_ID, PJ_NAME FROM PROJECT WHERE (PJ_ID = :PJ_ID);';
  SQLReadListProject = 'SELECT PJ_ID, PJ_NAME FROM PROJECT;';
  SQLCreateProject = 'INSERT INTO PROJECT (PJ_ID, PJ_NAME) VALUES (:PJ_ID, :PJ_NAME);';
  SQLDeleteProject = 'DELETE FROM PROJECT WHERE (PJ_ID = :PJ_ID);';
  SQLUpdateProject = 'UPDATE PROJECT SET PJ_NAME = :PJ_NAME WHERE (PJ_ID = :PJ_ID);';


{ TReadProjectVisitor }

Procedure TReadProjectVisitor.Init;

begin
  Query.SQLText:=SQLReadProject;
end;


Function TReadProjectVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TProject;
end;


Procedure TReadProjectVisitor.SetupParams;

var
  O : TProject;

begin
  O:=TProject(Visited);
  O.OID.AssignToTIQuery('PJ_ID',Query);
end;


Procedure TReadProjectVisitor.MapRowToObject;

var
  O : TProject;

begin
  O:=TProject(Visited);
  With Query do
    begin
    O.OID.AssignFromTIQuery('PJ_ID',Query);
    O.Name:=FieldAsString['PJ_NAME'];
    end;
end;


{ TReadListProjectVisitor }

Procedure TReadListProjectVisitor.Init;

begin
  Query.SQLText:=SQLReadListProject;
end;


Function TReadListProjectVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TProjectList;
end;


Procedure TReadListProjectVisitor.SetupParams;

begin
end;


Procedure TReadListProjectVisitor.MapRowToObject;

var
  O : TProject;

begin
  O:=TProject.Create;
  With Query do
    begin
    O.OID.AssignFromTIQuery('PJ_ID',Query);
    O.Name:=FieldAsString['PJ_NAME'];
    end;
  O.ObjectState:=posClean;
  TProjectList(Visited).Add(O);
end;


{ TSaveProjectVisitor }

Procedure TSaveProjectVisitor.Init;

begin
  Case Visited.ObjectState of
    posCreate:
      Query.SQLText:=SQLCreateProject;
    posUpdate:
      Query.SQLText:=SQLUpdateProject;
    posDelete:
      Query.SQLText:=SQLDeleteProject;
  end;
end;


Function TSaveProjectVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TProject;
  Result:=Result and (Visited.ObjectState in [posCreate,posdelete,posUpdate]);
end;


Procedure TSaveProjectVisitor.SetupParams;

var
  O : TProject;

begin
  O:=TProject(Visited);
  With Query do
    begin
    O.OID.AssignToTIQuery('PJ_ID',Query);
    If (Visited.ObjectState<>posDelete) then
      begin
      ParamAsString['PJ_NAME']:=O.Name;
      end;
    end;
end;

Const
  SQLReadPerson = 'SELECT PE_FIRSTNAME, PE_ID, PE_LASTNAME FROM PERSON WHERE (PE_ID = :PE_ID);';
  SQLReadListPerson = 'SELECT PE_FIRSTNAME, PE_ID, PE_LASTNAME FROM PERSON;';
  SQLCreatePerson = 'INSERT INTO PERSON (PE_FIRSTNAME, PE_ID, PE_LASTNAME) VALUES (:PE_FIRSTNAME, :PE_ID, :PE_LASTNAME);';
  SQLDeletePerson = 'DELETE FROM PERSON WHERE (PE_ID = :PE_ID);';
  SQLUpdatePerson = 'UPDATE PERSON SET PE_FIRSTNAME = :PE_FIRSTNAME, PE_LASTNAME = :PE_LASTNAME WHERE (PE_ID = :PE_ID);';

{ TReadPersonVisitor }

Procedure TReadPersonVisitor.Init;

begin
  Query.SQLText:=SQLReadPerson;
end;


Function TReadPersonVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TPerson;
end;


Procedure TReadPersonVisitor.SetupParams;

var
  O : TPerson;

begin
  O:=TPerson(Visited);
  O.OID.AssignToTIQuery('PE_ID',Query);
end;


Procedure TReadPersonVisitor.MapRowToObject;

var
  O : TPerson;

begin
  O:=TPerson(Visited);
  With Query do
    begin
    O.FirstName:=FieldAsString['PE_FIRSTNAME'];
    O.OID.AssignFromTIQuery('PE_ID',Query);
    O.LastName:=FieldAsString['PE_LASTNAME'];
    end;
end;


{ TReadListPersonVisitor }

Procedure TReadListPersonVisitor.Init;

begin
  Query.SQLText:=SQLReadListPerson;
end;


Function TReadListPersonVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TPersonList;
end;


Procedure TReadListPersonVisitor.SetupParams;

begin
end;


Procedure TReadListPersonVisitor.MapRowToObject;

var
  O : TPerson;

begin
  O:=TPerson.Create;
  With Query do
    begin
    O.FirstName:=FieldAsString['PE_FIRSTNAME'];
    O.OID.AssignFromTIQuery('PE_ID',Query);
    O.LastName:=FieldAsString['PE_LASTNAME'];
    end;
  O.ObjectState:=posClean;
  TPersonList(Visited).Add(O);
end;


{ TSavePersonVisitor }

Procedure TSavePersonVisitor.Init;

begin
  Case Visited.ObjectState of
    posCreate:
      Query.SQLText:=SQLCreatePerson;
    posUpdate:
      Query.SQLText:=SQLUpdatePerson;
    posDelete:
      Query.SQLText:=SQLDeletePerson;
  end;
end;


Function TSavePersonVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TPerson;
  Result:=Result and (Visited.ObjectState in [posCreate,posdelete,posUpdate]);
end;


Procedure TSavePersonVisitor.SetupParams;

var
  O : TPerson;

begin
  O:=TPerson(Visited);
  With Query do
    begin
    O.OID.AssignToTIQuery('PE_ID',Query);
    If (Visited.ObjectState<>posDelete) then
      begin
      ParamAsString['PE_FIRSTNAME']:=O.FirstName;
      ParamAsString['PE_LASTNAME']:=O.LastName;
      end;
    end;
end;

Const
  SQLReadTodo = 'SELECT TO_CREATEDON, TO_DONE, TO_ID, TO_PROJECT_FK, TO_REMARK, TO_TITLE FROM TODO WHERE (TO_ID = :TO_ID);';
  SQLReadListTodo = 'SELECT TO_CREATEDON, TO_DONE, TO_ID, TO_PROJECT_FK, TO_REMARK, TO_TITLE FROM TODO;';
  SQLCreateTodo = 'INSERT INTO TODO (TO_CREATEDON, TO_DONE, TO_ID, TO_PROJECT_FK, TO_REMARK, TO_TITLE) VALUES (:TO_CREATEDON, :TO_DONE, :TO_ID, :TO_PROJECT_FK, :TO_REMARK, :TO_TITLE);';
  SQLDeleteTodo = 'DELETE FROM TODO WHERE (TO_ID = :TO_ID);';
  SQLUpdateTodo = 'UPDATE TODO SET TO_CREATEDON = :TO_CREATEDON, TO_DONE = :TO_DONE, TO_PROJECT_FK = :TO_PROJECT_FK, TO_REMARK = :TO_REMARK, TO_TITLE = :TO_TITLE WHERE (TO_ID = :TO_ID);';

{ TReadodoVisitor }

Procedure TReadTodoVisitor.Init;

begin
  Query.SQLText:=SQLReadTodo;
end;


Function TReadTodoVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TTodo;
end;


Procedure TReadTodoVisitor.SetupParams;

var
  O : TTodo;

begin
  O:=TTodo(Visited);
  O.OID.AssignToTIQuery('TO_ID',Query);
end;


Procedure TReadTodoVisitor.MapRowToObject;

var
  O : TTodo;

begin
  O:=TTodo(Visited);
  With Query do
    begin
    O.CreatedOn:=FieldAsDateTime['TO_CREATEDON'];
    O.Done:=FieldAsString['TO_DONE']='1';
    O.OID.AssignFromTIQuery('TO_ID',Query);
    // Add custom loading code here for Project from TO_PROJECT_FK
    If Assigned(LoadProjectsList) then
      O.Project:=LoadProjectsList.Find(FieldAsString['TO_PROJECT_FK']) as TProject
    else
      begin
      O.Project:=TProject.Create;
      O.Project.OID.AssignFromTIQuery('TO_PROJECT_FK',Query);
      O.Project.ObjectState:=posPK;
      end;
    O.Remark:=FieldAsString['TO_REMARK'];
    O.Title:=FieldAsString['TO_TITLE'];
    end;
end;


{ TReadListodoVisitor }

Procedure TReadListTodoVisitor.Init;

begin
  Query.SQLText:=SQLReadListTodo;
end;


Function TReadListTodoVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TTodoList;
end;


Procedure TReadListTodoVisitor.SetupParams;

begin
end;


Procedure TReadListTodoVisitor.MapRowToObject;

var
  O : TTodo;

begin
  O:=TTodo.Create;
  With Query do
    begin
    O.CreatedOn:=FieldAsDateTime['TO_CREATEDON'];
    O.Done:=FieldAsString['TO_DONE']='1';
    O.OID.AssignFromTIQuery('TO_ID',Query);
    If Assigned(LoadProjectsList) then
      O.Project:=LoadProjectsList.Find(FieldAsString['TO_PROJECT_FK']) as TProject
    else
      begin
      O.Project:=TProject.Create;
      O.Project.OID.AssignFromTIQuery('TO_PROJECT_FK',Query);
      O.Project.ObjectState:=posPK;
      end;
    O.Remark:=FieldAsString['TO_REMARK'];
    O.Title:=FieldAsString['TO_TITLE'];
    end;
  O.ObjectState:=posClean;
  TTodoList(Visited).Add(O);
end;


{ TSaveodoVisitor }

Procedure TSaveTodoVisitor.Init;

begin
  Case Visited.ObjectState of
    posCreate:
      Query.SQLText:=SQLCreateTodo;
    posUpdate:
      Query.SQLText:=SQLUpdateTodo;
    posDelete:
      Query.SQLText:=SQLDeleteTodo;
  end;
end;


Function TSaveTodoVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TTodo;
  Result:=Result and (Visited.ObjectState in [posCreate,posdelete,posUpdate]);
end;


Procedure TSaveTodoVisitor.SetupParams;

var
  O : TTodo;

begin
  O:=TTodo(Visited);
  With Query do
    begin
    O.OID.AssignToTIQuery('TO_ID',Query);
    If (Visited.ObjectState<>posDelete) then
      begin
      ParamAsDateTime['TO_CREATEDON']:=O.CreatedOn;
      ParamAsString['TO_DONE']:=IntToStr(Ord(O.Done));
      If Assigned(O.Project) then
        O.Project.OID.AssignToTIQuery('TO_PROJECT_FK',Query);
      ParamAsString['TO_REMARK']:=O.Remark;
      ParamAsString['TO_TITLE']:=O.Title;
      end;
    end;
end;


Const
  SQLReadTask = 'SELECT TA_CREATEDON, TA_STATUS, TA_ID, TA_PROJECT_FK, TA_DESCRIPTION, TA_TITLE, TA_BUGID FROM Task WHERE (TA_ID = :TA_ID);';
  SQLReadListTask = 'SELECT TA_CREATEDON, TA_STATUS, TA_ID, TA_PROJECT_FK, TA_DESCRIPTION, TA_TITLE, TA_BUGID FROM Task;';
  SQLCreateTask = 'INSERT INTO Task (TA_CREATEDON, TA_STATUS, TA_ID, TA_PROJECT_FK, TA_DESCRIPTION, TA_TITLE, TA_BUGID) VALUES (:TA_CREATEDON, :TA_STATUS, :TA_ID, :TA_PROJECT_FK, :TA_DESCRIPTION, :TA_TITLE, :TA_BUGID);';
  SQLDeleteTask = 'DELETE FROM Task WHERE (TA_ID = :TA_ID);';
  SQLUpdateTask = 'UPDATE Task SET TA_CREATEDON = :TA_CREATEDON, TA_STATUS = :TA_STATUS, TA_PROJECT_FK = :TA_PROJECT_FK, TA_BUGID = :TA_BUGID, TA_DESCRIPTION = :TA_DESCRIPTION, TA_TITLE = :TA_TITLE WHERE (TA_ID = :TA_ID);';

{ TReadodoVisitor }

Procedure TReadTaskVisitor.Init;

begin
  Query.SQLText:=SQLReadTask;
end;


Function TReadTaskVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TTask;
end;


Procedure TReadTaskVisitor.SetupParams;

var
  O : TTask;

begin
  O:=TTask(Visited);
  O.OID.AssignToTIQuery('TA_ID',Query);
end;


Procedure TReadTaskVisitor.MapRowToObject;

var
  O : TTask;

begin
  O:=TTask(Visited);
  With Query do
    begin
    O.CreatedOn:=FieldAsDateTime['TA_CREATEDON'];
    O.Status:=TTaskStatus(FieldAsInteger['TA_STATUS']);
    O.BugID:=FieldAsInteger['TA_BUGID'];
    O.OID.AssignFromTIQuery('TA_ID',Query);
    // Add custom loading code here for Project from TA_PROJECT_FK
    If Assigned(LoadProjectsList) then
      O.Project:=LoadProjectsList.Find(FieldAsString['TA_PROJECT_FK']) as TProject
    else
      begin
      O.Project:=TProject.Create;
      O.Project.OID.AssignFromTIQuery('TA_PROJECT_FK',Query);
      O.Project.ObjectState:=posPK;
      end;
    O.Description:=FieldAsString['TA_DESCRIPTION'];
    O.Title:=FieldAsString['TA_TITLE'];
    end;
end;


{ TReadLisTaskVisitor }

Procedure TReadListTaskVisitor.Init;

begin
  Query.SQLText:=SQLReadListTask;
end;


Function TReadListTaskVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TTaskList;
end;


Procedure TReadListTaskVisitor.SetupParams;

begin
end;


Procedure TReadListTaskVisitor.MapRowToObject;

var
  O : TTask;

begin
  O:=TTask.Create;
  With Query do
    begin
    O.CreatedOn:=FieldAsDateTime['TA_CREATEDON'];
    O.Status:=TTaskStatus(FieldAsInteger['TA_STATUS']);
    O.OID.AssignFromTIQuery('TA_ID',Query);
    If Assigned(LoadProjectsList) then
      O.Project:=LoadProjectsList.Find(FieldAsString['TA_PROJECT_FK']) as TProject
    else
      begin
      O.Project:=TProject.Create;
      O.Project.OID.AssignFromTIQuery('TA_PROJECT_FK',Query);
      O.Project.ObjectState:=posPK;
      end;
    O.Description:=FieldAsString['TA_DESCRIPTION'];
    O.Title:=FieldAsString['TA_TITLE'];
    O.BugID:=FieldAsInteger['TA_BUGID'];
    end;
  O.ObjectState:=posClean;
  TTaskList(Visited).Add(O);
end;


{ TSaveodoVisitor }

Procedure TSaveTaskVisitor.Init;

begin
  Case Visited.ObjectState of
    posCreate:
      Query.SQLText:=SQLCreateTask;
    posUpdate:
      Query.SQLText:=SQLUpdateTask;
    posDelete:
      Query.SQLText:=SQLDeleteTask;
  end;
end;


Function TSaveTaskVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TTask;
  Result:=Result and (Visited.ObjectState in [posCreate,posdelete,posUpdate]);
end;


Procedure TSaveTaskVisitor.SetupParams;

var
  O : TTask;

begin
  O:=TTask(Visited);
  With Query do
    begin
    O.OID.AssignToTIQuery('TA_ID',Query);
    If (Visited.ObjectState<>posDelete) then
      begin
      ParamAsDateTime['TA_CREATEDON']:=O.CreatedOn;
      ParamAsInteger['TA_STATUS']:=Ord(O.Status);
      If Assigned(O.Project) then
        O.Project.OID.AssignToTIQuery('TA_PROJECT_FK',Query);
      ParamAsString['TA_DESCRIPTION']:=O.Description;
      ParamAsInteger['TA_BUGID']:=O.BugID;
      ParamAsString['TA_TITLE']:=O.Title;
      end;
    end;
end;

Const
  SQLReadInterrupt = 'SELECT IR_DURATION, IR_ID, IR_PERSON_FK, IR_TIMESTAMP FROM INTERRUPT WHERE (IR_ID = :IR_ID);';
  SQLReadListInterrupt = 'SELECT IR_DURATION, IR_ID, IR_PERSON_FK, IR_TIMESTAMP FROM INTERRUPT WHERE (IR_TIMESTAMP>=:Since);';
  SQLCreateInterrupt = 'INSERT INTO INTERRUPT (IR_DURATION, IR_ID, IR_PERSON_FK, IR_TIMESTAMP) VALUES (:IR_DURATION, :IR_ID, :IR_PERSON_FK, :IR_TIMESTAMP);';
  SQLDeleteInterrupt = 'DELETE FROM INTERRUPT WHERE (IR_ID = :IR_ID);';
  SQLUpdateInterrupt = 'UPDATE INTERRUPT SET IR_DURATION = :IR_DURATION, IR_PERSON_FK = :IR_PERSON_FK, IR_TIMESTAMP = :IR_TIMESTAMP WHERE (IR_ID = :IR_ID);';

{ TReadInterruptVisitor }

Procedure TReadInterruptVisitor.Init;

begin
  Query.SQLText:=SQLReadInterrupt;
end;


Function TReadInterruptVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TInterrupt;
end;


Procedure TReadInterruptVisitor.SetupParams;

var
  O : TInterrupt;

begin
  O:=TInterrupt(Visited);
  O.OID.AssignToTIQuery('IR_ID',Query);
end;


Procedure TReadInterruptVisitor.MapRowToObject;

var
  O : TInterrupt;

begin
  O:=TInterrupt(Visited);
  With Query do
    begin
    O.Duration:=FieldAsDateTime['IR_DURATION'];
    O.OID.AssignFromTIQuery('IR_ID',Query);
    If Assigned(LoadPersonsList) then
      O.Person:=LoadPersonsList.Find(FieldAsString['IR_PERSON_FK']) as TPerson
    else
      begin
      O.Person:=TPerson.Create;
      O.Person.OID.AssignFromTiQuery('IR_PERSON_FK',Query);
      O.Person.ObjectState:=posPK;
      end;
    O.TimeStamp:=FieldAsDateTime['IR_TIMESTAMP'];
    end;
end;


{ TReadListInterruptVisitor }

Procedure TReadListInterruptVisitor.Init;

begin
  Query.SQLText:=SQLReadListInterrupt;
end;


Function TReadListInterruptVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TInterruptList;
end;


Procedure TReadListInterruptVisitor.SetupParams;

begin
  Query.ParamAsDateTime['Since']:=(Visited as TInterruptList).Since;
end;


Procedure TReadListInterruptVisitor.MapRowToObject;

var
  O : TInterrupt;

begin
  O:=TInterrupt.Create;
  With Query do
    begin
    O.Duration:=FieldAsDateTime['IR_DURATION'];
    O.OID.AssignFromTIQuery('IR_ID',Query);
    If Assigned(LoadPersonsList) then
      O.Person:=LoadPersonsList.Find(FieldAsString['IR_PERSON_FK']) as TPerson
    else
      begin
      O.Person:=TPerson.Create;
      O.Person.OID.AssignFromTiQuery('IR_PERSON_FK',Query);
      O.Person.ObjectState:=posPK;
      end;
    O.TimeStamp:=FieldAsDateTime['IR_TIMESTAMP'];
    end;
  O.ObjectState:=posClean;
  TInterruptList(Visited).Add(O);
end;


{ TSaveInterruptVisitor }

Procedure TSaveInterruptVisitor.Init;

begin
  Case Visited.ObjectState of
    posCreate:
      Query.SQLText:=SQLCreateInterrupt;
    posUpdate:
      Query.SQLText:=SQLUpdateInterrupt;
    posDelete:
      Query.SQLText:=SQLDeleteInterrupt;
  end;
end;


Function TSaveInterruptVisitor.AcceptVisitor : Boolean;

begin
  Result:=Visited is TInterrupt;
  Result:=Result and (Visited.ObjectState in [posCreate,posdelete,posUpdate]);
end;


Procedure TSaveInterruptVisitor.SetupParams;

var
  O : TInterrupt;

begin
  O:=TInterrupt(Visited);
  With Query do
    begin
    O.OID.AssignToTIQuery('IR_ID',Query);
    If (Visited.ObjectState<>posDelete) then
      begin
      ParamAsDateTime['IR_DURATION']:=O.Duration;
      // Add custom loading code here for Person from IR_PERSON_FK
      ParamAsDateTime['IR_TIMESTAMP']:=O.TimeStamp;
      O.Person.OID.AssignToTIQuery('IR_PERSON_FK',Query);
      end;
    end;
end;

{ TTimeTrack }

Const
 SQLReadTimeTrack = 'SELECT TT_ID, TT_TASK_FK, TT_START, TT_STOP FROM TIMETRACK WHERE (TT_ID = :TT_ID);';
 SQLReadListTimeTrack = 'SELECT TT_ID, TT_TASK_FK, TT_START, TT_STOP FROM TIMETRACK WHERE (TT_START>=:Since);';
 SQLReadTaskTimeTrack = 'SELECT TT_ID, TT_START, TT_STOP FROM TIMETRACK WHERE (TT_TASK_FK=:TA_ID);';
 SQLCreateTimeTrack = 'INSERT INTO TIMETRACK (TT_ID, TT_TASK_FK, TT_START, TT_STOP) VALUES (:TT_ID, :TT_TASK_FK, :TT_START, :TT_STOP);';
 SQLDeleteTimeTrack = 'DELETE FROM TIMETRACK WHERE (TT_ID = :TT_ID);';
 SQLUpdateTimeTrack = 'UPDATE TIMETRACK SET TT_TASK_FK = :TT_TASKT_FK, TT_START = :TT_START, TT_STOP = :TT_STOP WHERE (TT_ID = :TT_ID);';

 { TReadimeTrackVisitor }

 Procedure TReadTimeTrackVisitor.Init;

 begin
   Query.SQLText:=SQLReadTimeTrack;
 end;


 Function TReadTimeTrackVisitor.AcceptVisitor : Boolean;

 begin
   Result:=Visited is TTimeTrack;
 end;


 Procedure TReadTimeTrackVisitor.SetupParams;

 var
   O : TTimeTrack;

 begin
   O:=TTimeTrack(Visited);
   O.OID.AssignToTIQuery('TT_ID',Query);
 end;


 Procedure TReadTimeTrackVisitor.MapRowToObject;

 var
   O : TTimeTrack;

 begin
   O:=TTimeTrack(Visited);
   With Query do
     begin
     O.OID.AssignFromTIQuery('TT_ID',Query);
     If Assigned(LoadTasksList) then
       O.Task:=LoadTasksList.Find(FieldAsString['TT_TASK_FK']) as TTask
     else
       begin
       O.Task:=TTask.Create;
       O.Task.OID.AssignFromTIQuery('TT_TASK_FK',Query);
       O.Task.ObjectState:=posPK;
       end;
     O.Start:=FieldAsDateTime['TT_START'];
     O.Stop:=FieldAsDateTime['TT_STOP'];
     end;
 end;


 { TReadListimeTrackVisitor }

 Procedure TReadListTimeTrackVisitor.Init;

 begin
   Query.SQLText:=SQLReadListTimeTrack;
 end;


 Function TReadListTimeTrackVisitor.AcceptVisitor : Boolean;

 begin
   Result:=Visited is TTimeTrackList;
 end;


 Procedure TReadListTimeTrackVisitor.SetupParams;

 begin
   Query.ParamAsDateTime['Since']:=TTimeTrackList(Visited).Since;
 end;


 Procedure TReadListTimeTrackVisitor.MapRowToObject;

 var
   O : TTimeTrack;

 begin
   O:=TTimeTrack.Create;
   With Query do
     begin
     O.OID.AssignFromTIQuery('TT_ID',Query);
     If Assigned(LoadTasksList) then
       O.Task:=LoadTasksList.Find(FieldAsString['TT_TASK_FK']) as TTask
     else
       begin
       O.Task:=TTask.Create;
       O.Task.OID.AssignFromTIQuery('TT_TASK_FK',Query);
       O.Task.ObjectState:=posPK;
       end;
     O.Start:=FieldAsDateTime['TT_START'];
     O.Stop:=FieldAsDateTime['TT_STOP'];
     end;
   O.ObjectState:=posClean;
   TTimeTrackList(Visited).Add(O);
 end;

 { TReadTaskTimeTrackVisitor }

 procedure TReadTaskTimeTrackVisitor.Init;
 begin
   Query.SQLText:=SQLReadTaskTimeTrack;
 end;

 function TReadTaskTimeTrackVisitor.AcceptVisitor: Boolean;
 begin
   Result:=Visited is TTaskTimeTrackList;
 end;

 procedure TReadTaskTimeTrackVisitor.SetupParams;
 begin
   TTaskTimeTrackList(Visited).Task.OID.AssignToTIQuery('TA_ID',Query);
  end;

 procedure TReadTaskTimeTrackVisitor.MapRowToObject;

 var
   O : TTimeTrack;

  begin
    O:=TTimeTrack.Create;
    With Query do
      begin
      O.OID.AssignFromTIQuery('TT_ID',Query);
      O.Task:=TTaskTimeTrackList(Visited).Task;
      O.Start:=FieldAsDateTime['TT_START'];
      O.Stop:=FieldAsDateTime['TT_STOP'];
      end;
    O.ObjectState:=posClean;
    TTaskTimeTrackList(Visited).Add(O);
 end;


 { TSaveimeTrackVisitor }

 Procedure TSaveTimeTrackVisitor.Init;

 begin
   Case Visited.ObjectState of
     posCreate:
       Query.SQLText:=SQLCreateTimeTrack;
     posUpdate:
       Query.SQLText:=SQLUpdateTimeTrack;
     posDelete:
       Query.SQLText:=SQLDeleteTimeTrack;
   end;
 end;


 Function TSaveTimeTrackVisitor.AcceptVisitor : Boolean;

 begin
   Result:=Visited is TTimeTrack;
   Result:=Result and (Visited.ObjectState in [posCreate,posdelete,posUpdate]);
 end;


 Procedure TSaveTimeTrackVisitor.SetupParams;

 var
   O : TTimeTrack;

 begin
   O:=TTimeTrack(Visited);
   With Query do
     begin
     O.OID.AssignToTIQuery('TT_ID',Query);
     If (Visited.ObjectState<>posDelete) then
       begin
       O.Task.OID.AssignToTiQuery('TT_TASK_FK',Query);
       ParamAsDateTime['TT_START']:=O.Start;
       ParamAsDateTime['TT_STOP']:=O.Stop;
       end;
     end;
 end;

Procedure RegisterVisitors;

begin
  RegisterVisitorsForClass(TPerson,TSavePersonVisitor,TSavePersonVisitor,TSavePersonVisitor,TReadPersonVisitor);
  RegisterVisitorsForClass(TProject,TSaveProjectVisitor,TSaveProjectVisitor,TSaveProjectVisitor,TReadProjectVisitor);
  RegisterVisitorsForClass(TTodo,TSaveTodoVisitor,TSaveTodoVisitor,TSaveTodoVisitor,TReadTodoVisitor);
  RegisterVisitorsForClass(TTask,TSaveTaskVisitor,TSaveTaskVisitor,TSaveTaskVisitor,TReadTaskVisitor);
  RegisterVisitorsForClass(TTimeTrack,TSaveTimeTrackVisitor,TSaveTimeTrackVisitor,TSaveTimeTrackVisitor,TReadTimeTrackVisitor);
  RegisterVisitorsForClass(TInterrupt,TSaveInterruptVisitor,TSaveInterruptVisitor,TSaveInterruptVisitor,TReadInterruptVisitor);
  RegisterVisitorForClass(TReadListPersonVisitor,TPersonList,vtRead);
  RegisterVisitorForClass(TReadListProjectVisitor,TProjectList,vtRead);
  RegisterVisitorForClass(TReadListTodoVisitor,TTodoList,vtRead);
  RegisterVisitorForClass(TReadListTaskVisitor,TTaskList,vtRead);
  RegisterVisitorForClass(TReadListTimeTrackVisitor,TTimeTrackList,vtRead);
  RegisterVisitorForClass(TReadTaskTimeTrackVisitor,TTaskTimeTrackList,vtRead);
  RegisterVisitorForClass(TReadListInterruptVisitor,TInterruptList,vtRead);
end;

initialization
  RegisterVisitors;
end.

