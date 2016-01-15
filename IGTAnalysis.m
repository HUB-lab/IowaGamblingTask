function DrugStroopAnalysis(filename)

    fclose('all');
    %filename='DS_99999999_9_99999999_9999_011016_2051.csv';
    formatSpec = '%s%f%s%f%f%f%f%f%f%f%f%*s%*s%*s%f%s';
    fileID = fopen(filename);
    C = textscan(fileID,formatSpec,'Delimiter',',','HeaderLines',1);
    fclose(fileID);

    trialtype = C{1};
    blocknum = C{2};
    word = C{3};
    wordisold = C{4};
    RESP = C{5};
    RT = C{6};
    ACC = C{7};
    trialinblock = C{8};
    CRESPrepeat = C{9};
    CRESP = C{10};
    breaktrials = textscan(sprintf('%s\n',C{end}{:}),'%f%f');
    for n=1:length(breaktrials)
       temp(n) = breaktrials{n}(1);
    end
    clear breaktrials
    breaktrials = temp;


    NeutralRT = mean(RT(ACC==1 .* strcmp(trialtype,'Neut') .* RT>.125)); %RT on correct trials only, exclude impossibly fast RTs
    DrugRT = mean(RT(ACC==1 .* strcmp(trialtype,'Drug') .* RT>.125)); %RT on correct trials only, exclude impossibly fast RTs
    DrugMinusNeutralRT = DrugRT-NeutralRT;

    NeutralACC = mean(ACC(strcmp(trialtype,'Neut')));
    DrugACC = mean(ACC(strcmp(trialtype,'Drug')));
    DrugMinusNeutralACC = DrugACC-NeutralACC;

    FirstTrialNeutralRT = mean(RT(trialinblock==1 .* ACC==1 .* strcmp(trialtype,'Neut') .* RT>.125)); %RT on correct trials only, exclude impossibly fast RTs
    FirstTrialDrugRT = mean(RT(trialinblock==1 .* ACC==1 .* strcmp(trialtype,'Drug') .* RT>.125)); %RT on correct trials only, exclude impossibly fast RTs
    FirstTrialDrugMinusNeutralRT = FirstTrialDrugRT-FirstTrialNeutralRT;

    %subset them to only correct trials
    correctidx = find(ACC==1);

    %construct our regressors
    clear X; clear Y;
    X(:,1) = strcmp(trialtype(correctidx),'Drug')-.5;                           %Main effect        [Drug>Neutral]
    X(:,2) = (CRESPrepeat(correctidx)==0)-.5;                                   %Main effect        [RespSwitch>Repeat]
    X(:,3) = mean(log(trialinblock(correctidx)))-log(trialinblock(correctidx)); %Mnin effect        log(trial in block) (it's negative and mean-deviated)
    X(:,4) = X(:,1) .* X(:,2);                                                  %2-way Interaction  [Drug>Neutral]x[RespSwitch>Repeat]
    X(:,5) = X(:,1) .* X(:,3);                                                  %2-way Interaction  [Drug>Neutral]x[LogTrialinBlock]
    X(:,6) = X(:,2) .* X(:,3);                                                  %2-way Interaction  [RespSwitch>Repeat]x[LogTrialinBlock]
    X(:,7) = X(:,1) .* X(:,2) .* X(:,3);                                        %3-way Interaction  [Drug>Neutral]x[RespSwitch>Repeat]x[LogTrialinBlock]
    Y = RT(correctidx);
    PredictorLabels = {'Intercept','D>N','S>R','Log(TrialinBlock)','D>NxS>R','D>NxLogTiB','S>RxLogTiB','D>NxS>RxLogTiB'};

    %fit the model
    [b,dev,stats] = glmfit(X,Y);
    predictedY = Y-stats.resid;
    scatter(Y,predictedY)
    plot(Y); hold on; plot(predictedY);

    %%begin plotting
    smoothACC = smooth(ACC,0.1,'loess');
    
    nrowstoplot = 4;
    clear myrownames;
    subplot(nrowstoplot,2,1:2), plot(RT, 'Color', 'red'); hold on; plot(smoothACC, 'Color', 'black'); plot(.2*strcmp(trialtype,'Drug'),'Color','blue'); ylabel('RT (s)'); xlabel('Trial Number');
           title(char(filename),'Interpreter','none');
           ylim([0 2])
    legend('RT(s)','ACC (loess)','Drug Blocks','Location','northeast');

    subplot(nrowstoplot,2,3:4), plot(Y, 'Color', 'red'); hold on; plot(predictedY,'Color','blue'); ylabel('RT (s)'); xlabel('Trial Number');
           ylim([0 2])
    legend('Correct RT(s)','GLM Fit','Location','northeast');
    
    clear barplotdata_means;
    clear barplotdata_stderrs;
    myrownames = [' ',PredictorLabels,' '];
    for n=1:length(stats.beta)
        barplotdata_means{n} = stats.beta(n); %consider converting these to relative risk for interpretability
        barplotdata_stderr{n} = stats.se(n);         
    end
    subplot(nrowstoplot,2,5:6), errorbar(cell2mat(barplotdata_means),cell2mat(barplotdata_stderr),'.'); set(gca,'XtickLabel',myrownames); ylabel('Effect (s)'); refline(0,0);

    h2 = subplot(nrowstoplot,2,7:8);set(h2,'Visible','off')    
    text(0, 1, sprintf('\n\n%s\n%s\n%s\n', 'Power practice function; accuracy close to 1; increased RT during Drug Blocks','Tight errorbars on model parameter estimates'), 'Parent', h2);

    %save it to pdf
    h=gcf;
    set(h,'PaperOrientation','landscape');
    set(h,'PaperUnits','normalized');
    set(h,'PaperPosition', [0 0 1 1]);
    print(gcf, '-dpdf', strrep(filename,'.csv','.pdf'));    
    close(gcf)

    %%
    %Make Pfizer summary file
    PfizerSummary(1) = NeutralRT;
    PfizerSummary(2) = DrugRT;
    PfizerSummary(3) = DrugMinusNeutralRT;
    PfizerSummary(4) = NeutralACC;
    PfizerSummary(5) = DrugACC;
    PfizerSummary(6) = DrugMinusNeutralACC;
    PfizerSummary(7) = FirstTrialNeutralRT;
    PfizerSummary(8) = FirstTrialDrugRT;
    PfizerSummary(9) = FirstTrialDrugMinusNeutralRT;
    PfizerSummary(10:(9+length(stats.beta))) = stats.beta';
    Pfizerfilename = ['PfzSum_',filename];
    fid = fopen(Pfizerfilename, 'w');
    betanames = [PredictorLabels{1}];
    for n = 2:length(PredictorLabels)
        betanames = [betanames,',',PredictorLabels{n}];
    end
    fprintf(fid, '%s\n',['NeutralRT,DrugRT,DrugMinusNeutralRT,NeutralACC,DrugACC,DrugMinusNeutralACC,FirstTrialNeutralRT,FirstTrialDrugRT,FirstTrialDrugMinusNeutralRT,',betanames]);
    fclose(fid);
    dlmwrite(Pfizerfilename, PfizerSummary, '-append','delimiter', ',', 'precision','%.6f','newline','pc');    

    
    %% Zip and protect
    filestem = strrep(strrep(filename,'.csv',''),'DS_','');
    dos(['7z a DS_' filestem '.zip ' '*' filestem '*']);
	dos(['icacls DS_' filestem '.zip /deny *S-1-1-0:(DE)']); 
