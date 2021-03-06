function IGTAnalysis(filename)


    %please note that any analysis of mousetracking probably has to happen
    %offline, because the mousetracking data is too big to analyze while
    %subjects are sitting there waiting for the next task
    fclose('all');
    %filename='IGT_99990000_1_B7441004_1001_999999_9999.csv';
    formatSpec = '%f%f%f%f%f%f%f%f';
    fileID = fopen(filename);
    C = textscan(fileID,formatSpec,'Delimiter',',','HeaderLines',1);
    fclose(fileID);

    %trialnum,choice,rt,reward,penalty,netgain,previous_earnings,running_netscore
    trialnum = C{1};
    choice = C{2};
    RT = C{3};
    reward = C{4};
    penalty = C{5};
    netgain = C{6};
    previous_earnings = C{7};
    running_netscore = C{8};

    %calculate endpoints
    NetScoreOverall = sum(C{2}>2)-sum(C{2}<3);
    NetScore1to20 = sum(C{2}(1:20)>2)-sum(C{2}(1:20)<3);
    NetScore21to40 = sum(C{2}(21:40)>2)-sum(C{2}(21:40)<3);
    NetScore41to60 = sum(C{2}(41:60)>2)-sum(C{2}(41:60)<3);
    NetScore61to80 = sum(C{2}(61:80)>2)-sum(C{2}(61:80)<3);
    NetScore81toend = sum(C{2}(81:end)>2)-sum(C{2}(81:end)<3);
    
    
    
    
    %%begin plotting

    smoothnetscore = smooth(running_netscore,0.1,'loess');
    
    nrowstoplot = 4;
    clear myrownames;
    subplot(nrowstoplot,2,1:2), plot(RT, 'Color', 'red'); ylabel('RT (s)'); xlabel('Trial Number');
           title(char(filename),'Interpreter','none');
           ylim([0 2])

    subplot(nrowstoplot,2,3:4), plot(smoothnetscore, 'Color', 'red'); ylabel('Smooth NetScore'); xlabel('Trial Number');

    subplot(nrowstoplot,2,5:6), plot(reward, 'Color', 'green'); hold on; plot(penalty, 'Color', 'red'); ylabel('Outcome'); xlabel('Trial Number');
    legend('Reward','Penalty','Location','northeast');

    h2 = subplot(nrowstoplot,2,7:8);set(h2,'Visible','off')    
    text(0, 1, sprintf('\n\n%s\n%s\n%s\n', 'Exponential decrease in RT; increasing Netscore'), 'Parent', h2);

    %save it to pdf
    h=gcf;
    set(h,'PaperOrientation','landscape');
    set(h,'PaperUnits','normalized');
    set(h,'PaperPosition', [0 0 1 1]);
    print(gcf, '-dpdf', strrep(filename,'.csv','.pdf'));    
    close(gcf)

    %%
    %Make Pfizer summary file
    PfizerSummary(1) = NetScoreOverall;
    PfizerSummary(2) = NetScore1to20;
    PfizerSummary(3) = NetScore21to40;
    PfizerSummary(4) = NetScore41to60;
    PfizerSummary(5) = NetScore61to80;
    PfizerSummary(6) = NetScore81toend;
    Pfizerfilename = ['PfzSum_',filename];
    fid = fopen(Pfizerfilename, 'w');
    fprintf(fid, '%s\n',['NetScoreOverall,NetScore1to20,Netscore21to40,Netscore41to60,Netscore61to80,Netscore81toend']);
    fclose(fid);
    dlmwrite(Pfizerfilename, PfizerSummary, '-append','delimiter', ',', 'precision','%.6f','newline','pc');    

    
    %% Zip and protect
    filestem = strrep(strrep(filename,'.csv',''),'IGT','');
    dos(['7z a IGT_' filestem '.zip ' '*' filestem '*']);
