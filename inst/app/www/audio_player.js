function setUpAudioPlayer(playerId, iconId, imageId, spinnerId) {
  const player = document.getElementById(playerId);
  const icon = document.getElementById(iconId);
  const speciesImage = document.getElementById(imageId);
  const spinner = document.getElementById(spinnerId);

  if (!player || !icon || !speciesImage || !spinner) {
    console.log('One of the elements is missing:', playerId, iconId, imageId, spinnerId);
    return;
  }

  speciesImage.addEventListener('click', function () {
    console.log('Playing:', player.src);

    if (player.paused) {
      spinner.style.display = 'inline-block';
      console.log('Spinner should be visible now');
      player.play();
    } else {
      player.pause();
      icon.innerHTML = '<i class="bi bi-play-fill"></i>';
    }
  });

  player.addEventListener('playing', function () {
    spinner.style.display = 'none';
    icon.innerHTML = '<i class="bi bi-pause-fill"></i>';
    console.log('Audio is playing, spinner hidden');
  });

  player.addEventListener('pause', function () {
    icon.innerHTML = '<i class="bi bi-play-fill"></i>';
    console.log('Audio is paused');
  });

  player.addEventListener('ended', function () {
    icon.innerHTML = '<i class="bi bi-play-fill"></i>';
    console.log('Audio ended');
  });

  player.addEventListener('waiting', function () {
    spinner.style.display = 'inline-block';
    console.log('Audio is waiting, spinner should be visible');
  });

  player.addEventListener('canplay', function () {
    spinner.style.display = 'none';
    console.log('Audio can play, spinner hidden');
  });

  console.log('Audio player set up for:', playerId, iconId, imageId, spinnerId);
}

// Initialize all audio players
function initializePlayers() {
  const players = document.querySelectorAll('.detection_sound_image');
  for (const player of players) {
    const playerId = player.querySelector('audio').id;
    const iconId = player.querySelector('div.overlay_icon').id;
    const imageId = player.querySelector('img').id;
    const spinnerId = player.querySelector('div.spinner').id;
    console.log('Found player:', playerId); // Log player ID
    setUpAudioPlayer(playerId, iconId, imageId, spinnerId);
  }
  console.log("Players initialized!");
}

// Check initialization and ensure all elements are present before initializing players
function checkInitialization(targetLength) {
  const currentLength = document.querySelectorAll('.detection_sound_image').length;
  console.log(`Checking initialization: currentLength = ${currentLength}, targetLength = ${targetLength}`);

  if (currentLength >= targetLength) {
    initializePlayers();
    console.log("All elements are present. Players initialized!");
  } else {
    requestAnimationFrame(() => checkInitialization(targetLength));
    console.log("Players not found, rechecking...");
  }
}

$(document).ready(function() {
  console.log("Document ready, awaiting initialization signal...");
});

$(document).ready(function() {
  Shiny.addCustomMessageHandler('newCardsAdded', function(message) {
    console.log("Custom message received:", message);
    const targetLength = message.targetLength;
    checkInitialization(targetLength); // Use the checkInitialization function
    console.log("New cards added, initialization checking started!");
  });
});
