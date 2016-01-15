function [images] = IGTInstructions(w,images)

    %instructions adapted from http://behavioralandbrainfunctions.biomedcentral.com/articles/10.1186/1744-9081-3-37

    %% begin instructions
    DrawDecks(w,images,0); DrawColorBar(w,images,[],[],[],[]);
    inst = ['In front of you on the screen, there are four decks of cards A, B, C, and D.\n\n' ...
        'In this game you''ll select one card at a time, by clicking on the card, from any deck you choose.\n\n' ...
        'You will get $2000 credit (see the green bar) to start the game. At the end, we will see how much you won or lost.\n\n' ...
        'To continue the tutorial, please select Deck A now.'];
    DrawFormattedText(w,inst,'center',images.yres*(1/2),225,images.wrap);
    Screen('Flip',w);

    WaitSecs(0.5);
    selected_deck = 0;
    while ~(selected_deck==1)
        [x,y,buttons] = GetMouse;
        if buttons(1) % wait for left click
            for deck=1:4
                if IsInRect(x, y, images.position.deck{deck})
                    selected_deck = deck;
                end
            end
        end
    end
    
    inst = ['Each time you select a card from a deck, an outline appears around the card, and the computer will tell you that you won some money. \n\n'...
    'I won''t tell you how much money you will win. You will find out along the way. \n\n'...
    'Every time you win, the green bar gets longer.\n\n' ...
    'Press any button to see the green bar get longer.'];
    DrawDecks(w,images,selected_deck); DrawColorBar(w,images,'A',1000,[],[]);
    DrawFormattedText(w,inst,'center',images.yres*(1/2),225,images.wrap);
    Screen('Flip',w);
    WaitSecs(0.5); %wait so they can see the color bar
    KbWait(-1);

    images.earnings = 3000;        
    DrawDecks(w,images,selected_deck); DrawColorBar(w,images,'A',1000,[],[]);
    DrawFormattedText(w,inst,'center',images.yres*(1/2),225,images.wrap);
    Screen('Flip',w);
    WaitSecs(1); %wait so they can see the color bar

    inst = ['Every so often, however, when you click on a card, the computer tells you that you won some money, but then it says that you ALSO lost some money.\n\n'...
        'I won''t tell you when you will lose or how much you will lose. You will find out along the way. \n\n'...
        'Every time you lose, the green bar gets shorter.\n\n' ...
        'Press any button to see the green bar get shorter.'];
    DrawDecks(w,images,selected_deck); DrawColorBar(w,images,'A',1000,[],[]);
    DrawFormattedText(w,inst,'center',images.yres*(1/2),225,images.wrap);
    Screen('Flip',w);
    WaitSecs(0.5); %wait so they can see the color bar
    KbWait(-1);

    images.earnings = images.earnings-1000;
    selected_deck = 0;
    DrawDecks(w,images,selected_deck); DrawColorBar(w,images,'A',1000,-1000,0);
    DrawFormattedText(w,inst,'center',images.yres*(1/2),225,images.wrap);
    Screen('Flip',w);
    WaitSecs(1); %wait so they can see the color bar

    inst = ['You are absolutely free to switch from one deck to another any time you wish.\n\n' ...
        'The goal is to win as much money as possible. If you''re unable to win, make sure to avoid losing money as much as possible.\n\n' ...
        'Press any button to continue the instructions.'];
    DrawDecks(w,images,selected_deck); DrawColorBar(w,images,'A',1000,-1000,0);   
    DrawFormattedText(w,inst,'center',images.yres*(1/2),225,images.wrap);
    Screen('Flip',w);
    WaitSecs(0.5); %wait so they can read
    KbWait(-1);

    inst = ['I won''t tell you how long the game will continue. You must keep playing until the computer stops.\n\n' ...
        'Press any button to continue the instructions.'];
    DrawDecks(w,images,selected_deck); DrawColorBar(w,images,'A',1000,-1000,0);
    DrawFormattedText(w,inst,'center',images.yres*(1/2),225,images.wrap);
    Screen('Flip',w);
    WaitSecs(0.5); %wait so they can read
    KbWait(-1);
    
    inst = ['It is important to know that the computer does not make you lose money at random. \n\n'...
        'However, there is no way for you to figure out when the computer will make you lose. \n\n'...
        'Press any button to continue the instructions.'];
    DrawFormattedText(w,inst,'center','center',225,images.wrap);
    Screen('Flip',w);
    WaitSecs(0.5); %wait so they can read
    KbWait(-1);

    inst = ['You may find yourself losing money on all of the decks, but some decks will make you lose MORE than others. \n\n'...
        'You can win if you stay away from the worst decks. Press any button to continue the instructions.'];
    DrawFormattedText(w,inst,'center','center',225,images.wrap);
    Screen('Flip',w);
    WaitSecs(0.5); %wait so they can read
    KbWait(-1);
    
    inst = ['Do you have any questions? \n\n'...
        'When you are ready to begin the game, please click the mouse.'];
    DrawFormattedText(w,inst,'center','center',225,images.wrap);
    Screen('Flip',w);
    WaitSecs(0.5); %wait so they can read
    buttons(1)=0;
    while ~buttons(1)
        [x,y,buttons] = GetMouse;
    end
    
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
