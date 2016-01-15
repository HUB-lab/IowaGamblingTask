function IGTLauncher(subjectIDinput, centerIDinput, sessionIDinput, studyIDinput, mmddyy, HHMM)

% usage: IGTLauncher(99990000,1001,1,'B7441004','999999','9999')
% cd('C:\Users\CHATHC01\Documents\D3Project\Spivack\Tasks\IGT\IGT');

%     subjectIDinput=99990000;
%     sessionIDinput=1;
%     centerIDinput=1001;
%     studyIDinput='B7441004';
%     mmddyy = '050505';
%     HHMM = '9990';

    
try 
    %clear all
    ShowCursor;
    Screen('Preference', 'SkipSyncTests', 1);
    w=Screen('OpenWindow',0,[0 0 0]);
    [images] = IGTSetupImages(w); %read in the decks etc
    [images] = IGTInstructions(w,images); %commented out for testing
    [task, images, trials] = IGTTask(w,subjectIDinput,sessionIDinput,['IGT_' num2str(subjectIDinput) '_' num2str(sessionIDinput) '_' (studyIDinput) '_' num2str(centerIDinput) '_' mmddyy '_' HHMM],images);
    DrawFormattedText(w,'Thank you, this task is complete. Please get the experimenter.','center',0.5*images.yres,225,images.wrap);
    Screen('Flip',w); %display this while saving the IGT workspace, which may be very large
    eval(['save ' ['IGT_' num2str(subjectIDinput) '_' num2str(sessionIDinput) '_' (studyIDinput) '_' num2str(centerIDinput) '_' mmddyy '_' HHMM]]);
    fclose('all');
    sca
    IGTAnalysis(['IGT_' num2str(subjectIDinput) '_' num2str(sessionIDinput) '_' (studyIDinput) '_' num2str(centerIDinput) '_' mmddyy '_' HHMM '.csv']);
    exit;
catch err
    sca
    rethrow(err)
    eval(['save ' ['IGT_' num2str(subjectIDinput) '_' num2str(sessionIDinput) '_' (studyIDinput) '_' num2str(centerIDinput) '_' mmddyy '_' HHMM]]);
    fclose('all');
    exit;
end

