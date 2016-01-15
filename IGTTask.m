function [task, images, trials]=IGTTask(w,subjectIDinput,sessionIDinput,logfilename,images)

    rand('state',subjectIDinput); %every subject will have the same randomization across sessions

    % make logfile to be filled in in real time
    logfile = fopen([logfilename '.csv'], 'a');
    fprintf(logfile,'trialnum,choice,rt,reward,penalty,netgain,previous_earnings,running_netscore','\n');

    task.starttime = GetSecs(); 
    
    %setup Task variables for designing blocks and trials, inputs etc
    KbName('UnifyKeyNames');
    task.totaltrials = 100; %100 is standard; some papers go up to 150
    task.ITI = 0.5; %intertrial interval in secs
    task.outcomedisplaydur = 2; %how long in secs to display the outcome of their choice. This is crucial and should be long to allow subjects to register the impact of their choice on earnings. No less than 3s ITI is good if you want to measure pupil diameter.
    task.numchoices(1:4) = 0; %construct counter of the number of choices they've made from each deck 1-4
    
    %setup deck vectors from input file
    tempData = importdata('PayoffScheme.csv');
    for col=1:length(tempData.colheaders)
        decks.(tempData.colheaders{col})=tempData.data(:,col);
    end
    decks.Gains = [decks.DeckAGain, decks.DeckBGain, decks.DeckCGain, decks.DeckDGain];
    decks.Losses = [decks.DeckALoss, decks.DeckBLoss, decks.DeckCLoss, decks.DeckDLoss];

    % initialise data vectors
    trials.RESP = zeros(1,task.totaltrials);         % Response button pressed
    trials.RT = zeros(1,task.totaltrials);           % RT
    trials.trialstarttime = zeros(1,task.totaltrials); 
    trials(1).earnings = 2000; %start with $2000 borrowed
    
    %% Finally ready to begin task
        
    %prepare for trial loop and commence
    for currtrial = 1:task.totaltrials
        %trial setup - record time, reset mouse position for subsequent
        %mouse tracking, carryover earnings from last trial
        loopstarttime = GetSecs();
        trials(currtrial).mousepos.x = images.xres/2;
        trials(currtrial).mousepos.y = images.yres/2;
        SetMouse(.5*images.xres, .75*images.yres,w);
        trials(currtrial).selected_deck = 0;
        if currtrial==1
            trials(currtrial).earnings = 2000; %start with $2000 borrowed
        else
            trials(currtrial).earnings = trials(currtrial-1).earnings + trials(currtrial-1).netgain; %calculate what their earnings are going into this trial
        end
        
        %draw screen with earnings and blank everything else; this will appear onscreen until the ITI has elapsed
        DrawColorBar(w,images,[],[],[],[]);
        Screen('Flip',w);
        
        %draw stimulus decks and color bar on new Screen so we're prepared
        %to flip once ITI is elapsed
        DrawDecks(w,images,trials(currtrial).selected_deck); DrawColorBar(w,images,[],[],[],[]);
        
        %Wait for ITI to elapse
        while (GetSecs() < (loopstarttime  + task.ITI))
            WaitSecs(0.005); %5 milisecond resolution on ITI
        end

        %start the trial timestamp and show the stimulus display we drew
        trials(currtrial).starttime = GetSecs(); 
        Screen('Flip',w);
        timecounter = 0;
        
        while trials(currtrial).selected_deck==0
            timecounter = timecounter + 1;
            trials(currtrial).mousepos(timecounter).time = (GetSecs() - trials(currtrial).starttime);
            [trials(currtrial).mousepos(timecounter).x,trials(currtrial).mousepos(timecounter).y,trials(currtrial).buttons] = GetMouse;
            if any(trials(currtrial).buttons) % if a button is clicked
                for deck=1:4 %then loop through the 4 decks was clicked
                    if IsInRect(trials(currtrial).mousepos(timecounter).x, trials(currtrial).mousepos(timecounter).y, images.position.deck{deck}) %to figure out if one was clicked
                        trials(currtrial).RT = (GetSecs() - trials(currtrial).starttime);  %First, get RT
                        trials(currtrial).selected_deck = deck; %Record response
                        task.numchoices(trials(currtrial).selected_deck) = task.numchoices(trials(currtrial).selected_deck)+1; %increment counter of number of times pulled from this deck
                        trials(currtrial).RESP = trials(currtrial).selected_deck; %Store in trials struct
                    end
                end
                if trials(currtrial).selected_deck==0 %reset buttons if they clicked outside a deck, because then we don't care
                    trials(currtrial).buttons = 0 .* trials(currtrial).buttons;
                end
            end
        end

        %They've now selected a deck, so we can present the outcomes
        trials(currtrial).netscore = (task.numchoices(3) + task.numchoices(4)) - (task.numchoices(2) + task.numchoices(2)); %keep a running tally of the netscore
        trials(currtrial).choice = images.label{trials(currtrial).selected_deck};
        trials(currtrial).reward = decks.Gains(task.numchoices(trials(currtrial).selected_deck),trials(currtrial).selected_deck);
        trials(currtrial).penalty = decks.Losses(task.numchoices(trials(currtrial).selected_deck),trials(currtrial).selected_deck);
        trials(currtrial).netgain = trials(currtrial).reward + trials(currtrial).penalty;
        images.earnings = trials(currtrial).earnings + trials(currtrial).netgain; %note that images.earnings is what is shown to subjects after a choice; trial.earnings is how many earnings they are GOING INTO the next trial with
        DrawDecks(w,images,trials(currtrial).selected_deck); DrawColorBar(w,images,trials(currtrial).choice,trials(currtrial).reward,trials(currtrial).penalty,trials(currtrial).netgain);
        Screen('Flip',w);
                

        %Write to the log while we're waiting for the subjects to get their yummy dopamine
        %trialnum,choice,rt,reward,penalty,netgain,previous_earnings,running_netscore
        trials(currtrial).outline = strcat(num2str(currtrial),',',num2str(trials(currtrial).selected_deck),',',num2str(trials(currtrial).RT),',',num2str(trials(currtrial).reward),',',num2str(trials(currtrial).penalty),',',num2str(trials(currtrial).netgain),',',num2str(trials(currtrial).earnings),',',num2str(trials(currtrial).netscore));
        fprintf(logfile,([trials(currtrial).outline,'\n']));
        while (GetSecs()-(trials(currtrial).RT+trials(currtrial).starttime)) < task.outcomedisplaydur
            WaitSecs(0.005); %5ms time resolution on outcome display duration
        end

    end
    task.completetime = GetSecs();
    sca;
    fclose(logfile);
end   
    
 function DrawColorBar(w, images, Choice, Reward, Penalty, NetGain)
    Screen('DrawTexture',w,images.colorbartexture,[],images.position.colorbar);  %draw colorbar
    Screen('FillRect', w,0,[images.position.colorbar(1)+(((images.earnings+1000)/6000)*(images.position.colorbar(3)-images.position.colorbar(1))), 0.8*images.yres, images.position.colorbar(3), 0.9*images.yres]); %black mask over color bar
    DrawFormattedText(w,'-1000',(images.xres/3),0.9*images.yres,225,images.wrap);
    DrawFormattedText(w,'0',1.2*(images.xres/3),0.9*images.yres,225,images.wrap);
    DrawFormattedText(w,'1000',1.4*(images.xres/3),0.9*images.yres,225,images.wrap);
    DrawFormattedText(w,'2000',1.6*(images.xres/3),0.9*images.yres,225,images.wrap);
    DrawFormattedText(w,'3000',1.8*(images.xres/3),0.9*images.yres,225,images.wrap);    
    DrawFormattedText(w,'4000',2*(images.xres/3),0.9*images.yres,225,images.wrap);
    DrawFormattedText(w,'5000',2.2*(images.xres/3),0.9*images.yres,225,images.wrap);    
    DrawFormattedText(w,'5000',2.2*(images.xres/3),0.9*images.yres,225,images.wrap);    
    DrawFormattedText(w,['Total: ',num2str(images.earnings)],1.5*(images.xres/3),0.75*images.yres,225,images.wrap);    
    DrawFormattedText(w,['Choice: ',num2str(Choice),'\n\nReward: ',num2str(Reward),'\n\nPenalty: ',num2str(Penalty),'\n\nNet Gain: ',num2str(NetGain)],0.25*(images.xres/3),0.7*images.yres,225,images.wrap);    
end

function DrawDecks(w,images,selected_deck)
    Screen('FillRect', w,0,[]); %black mask over color bar
    Screen('DrawTexture',w,images.tablebackground,[],[0 0 images.xres images.yres*.75]); %draw background 
    for deck = 1:4
        DrawFormattedText(w,images.label{deck},'center','center',225,images.wrap,[],[],[],[],images.position.label{deck});
        if deck==selected_deck
            Screen('DrawTexture',w,images.selecteddecktexture,[],images.position.deck{deck});  %draw each deck
        else
            Screen('DrawTexture',w,images.decktexture,[],images.position.deck{deck});  %draw each deck
        end
    end        
end
